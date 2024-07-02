;;; migrator.lisp

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage :migrator
  (:use :cl)
  (:import-from :sqlite
                #:with-open-database
                #:execute-non-query
                #:execute-to-list
                #:with-transaction
                #:execute-single)
  (:export
   #:migrate))

(in-package :migrator)


(defun execute-script (db script)
  (when (probe-file script)
    (flet ((emptyp (string)
             (zerop (length string))))
      (with-transaction db
        (loop for statement in (uiop:split-string (uiop:read-file-string script) :separator '(#\;))
              when (not (emptyp (ppcre:regex-replace-all "\\s+" statement "")))
                do (execute-non-query db statement))))))


(defun migrate (db directory &key allow-deletions)
  (execute-script db (merge-pathnames "pre.sql" directory))
  (with-open-database (pristine ":memory:")
    (execute-script pristine (or (probe-file (merge-pathnames "schema.sql" directory))
                                 (error "No schema.sql found in ~A" directory)))
    (let ((n-changes 0))
      (labels ((exec (description sql &rest args)
                 (v:info :migrator "~A - ~A" description sql)
                 (apply 'execute-to-list db sql args)
                 (incf n-changes))
               (migrate-pragma (pragma)
                 (let ((pristine-val (execute-single pristine (format nil "PRAGMA ~A" pragma)))
                       (val (execute-single db (format nil "PRAGMA ~A" pragma))))
                   (when (/= pristine-val val)
                     (exec (format nil "Set ~A to ~D from ~D" pragma pristine-val val)
                           (format nil "PRAGMA ~A = ~D" pragma pristine-val)))
                   pristine-val)))
        (with-transaction db
          (let* ((pristine-tables (execute-to-list
                                   pristine
                                   "SELECT name, sql FROM sqlite_master WHERE type = \"table\" AND name != \"sqlite_sequence\""))
                 (tables (execute-to-list
                          db
                          "SELECT name, sql FROM sqlite_master WHERE type = \"table\" AND name != \"sqlite_sequence\""))
                 (new-tables (set-difference pristine-tables tables :test 'string= :key 'car))
                 (removed-tables (set-difference tables pristine-tables :test 'string= :key 'car))
                 (modified-tables (loop for (name sql) in pristine-tables
                                        when (string/=
                                              (cadr (assoc name tables :test 'string=))
                                              sql)
                                          collect name)))
            ;; This PRAGMA is automatically disavled when the db is committed.
            (execute-non-query db "PRAGMA defer_foreign_keys = TRUE")

            ;; New and removed tables are easy
            (loop for (name sql) in new-tables
                  do (exec (format nil "Create table ~A" name) sql))
            (if (and removed-tables
                     (not allow-deletions))
                (error "Refusing to delete tables.")
                (loop for (name sql) in removed-tables
                      do (exec (format nil "Drop table ~A" name)
                               (format nil "DROP TABLE ~A" name))))

            ;; The SQLite documentation insists that we create the new table and
            ;; rename it over the old rather than moving the old out of the way
            ;; and then creating the new
            (loop for tbl-name in modified-tables
                  do (let* ((create-table-sql (ppcre:regex-replace
                                               (format nil "\\b~A\\b" tbl-name)
                                               (second (assoc tbl-name pristine-tables :test #'string=))
                                               (format nil "~A_migration_new" tbl-name)))
                            (cols (loop for col in (execute-to-list
                                                    db
                                                    (format nil "PRAGMA table_info(~A)" tbl-name))
                                        collect (second col)))
                            (pristine-cols (loop for col in (execute-to-list
                                                             pristine
                                                             (format nil "PRAGMA table_info(~A)" tbl-name))
                                                 collect (second col)))
                            (removed-cols (set-difference cols pristine-cols :test #'string=)))
                       (exec (format nil "Columns change: Create table ~A with updated schema" tbl-name)
                             create-table-sql)
                       (if (and removed-cols (not allow-deletions))
                           (error "Refusuing to remove columns.")
                           (exec (format nil "Migrate data fro table ~A" tbl-name)
                                 (let ((common (intersection cols pristine-cols :test #'string=)))
                                   (format nil
                                           "INSERT INTO ~A_migration_new (~{~A~^, ~}) select ~{~A~^, ~} from ~A"
                                           tbl-name common common tbl-name))))
                       ;; Don't need the old table anymore
                       (exec (format nil "Drop old table ~A now data has been migrated" tbl-name)
                             (format nil "DROP TABLE ~A" tbl-name))

                       (exec (format nil "Columns change: Move new table ~A over old" tbl-name)
                             (format nil "ALTER TABLE ~A_migration_new RENAME TO ~A" tbl-name tbl-name))))
            ;; Migrate the indexes
            (let ((indexes (execute-to-list db "SELECT name,sql FROM sqlite_master where type=\"index\"  "))
                  (pristine-indexes (execute-to-list pristine "SELECT name,sql FROM sqlite_master where type=\"index\"")))
              (loop for (name sql) in (set-difference indexes pristine-indexes :test #'string= :key #'car)
                    do (exec (format nil "Dropping obsolete index ~A" name)
                             (format nil "DROP INDEX ~A" name)))
              (loop for (name sql) in pristine-indexes
                    when (not (member name indexes :test #'string= :key #'car))
                      do (exec (format nil "Creating new index ~A" name) sql)
                    end
                    when (string/= sql (second (assoc name indexes :test #'string=)))
                      do (progn
                           (exec (format nil "Index ~A changed: Dropping old version" name)
                                 (format nil "DROP INDEX ~A" name))
                           (exec (format nil "Index ~A changed: Creating updated version it its place" name) sql)))

              (migrate-pragma "user_version")
              (when (and (= 0 (execute-single pristine "PRAGMA foreign_keys"))
                         (execute-to-list db "PRAGMA foreign_key_check"))
                (error "Database migration: Would fail foreign_key_check")))))
        (migrate-pragma "foreign_keys")
        (when (> n-changes 0)
          (execute-non-query db "VACUUM"))
        (execute-script db (merge-pathnames "post.sql" directory))))))
