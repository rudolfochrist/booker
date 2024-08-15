;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:booker/db
  (:use :cl :booker :sxql)
  (:export
   #:all-bookmarks
   #:generate-db-id
   #:find-bookmark
   #:update-bookmark
   #:delete-bookmark
   #:create-bookmark
   #:search-bookmarks
   #:find-bookmark-with-body
   #:migrate
   #:status
   #:new-migration))

(in-package #:booker/db)

(defvar *dbi-pool*
  (anypool:make-pool :name "dbi-connections"
                     :connector (lambda ()
                                  (dbi:connect :sqlite3 :database-name booker::*database-name*))
                     :disconnector #'dbi:disconnect
                     :ping #'dbi:ping))

(defparameter *emacsclient-exec* "/Applications/MacPorts/Emacs.app/Contents/MacOS/bin/emacsclient")

(defun get-last-migration ()
  (anypool:with-connection (conn *dbi-pool*)
    (let ((datafly:*connection* conn))
      (datafly:retrieve-one
       (select (:id :kind :description :applied)
         (from :migration)
         (order-by (:desc :id))
         (limit 1))))))

(defun dump-schema ()
  (with-open-file (fd (booker::root "db/schema.sql") :direction :output :if-exists :supersede)
    (uiop:run-program (list "sqlite3" booker::*database-name* ".schema") :output fd)
    (let* ((last-migration (get-last-migration)))
      (format fd "~%-- cl-migration schema migrations~%~
INSERT INTO migration (id, kind, description, applied) VALUES (~A, '~A', '~A', '~A');~%"
              (getf last-migration :id)
              (getf last-migration :kind)
              (getf last-migration :description)
              (getf last-migration :applied)))))

(defun migrate ()
  (anypool:with-connection (conn *dbi-pool*)
    (let* ((provider (migratum.provider.local-path:make-provider
                      (list (asdf:system-relative-pathname "booker" "db/migrations/"))))
           (driver (migratum.driver.dbi:make-driver provider conn)))
      (migratum:provider-init provider)
      (migratum:driver-init driver)
      (unwind-protect (migratum:apply-pending driver)
        (migratum:provider-shutdown provider)
        (migratum:driver-shutdown driver))
      (dump-schema)
      (format t "~&Migration finished."))))

(defun status ()
  (anypool:with-connection (conn *dbi-pool*)
    (let* ((provider (migratum.provider.local-path:make-provider
                      (list (asdf:system-relative-pathname "booker" "db/migrations/"))))
           (driver (migratum.driver.dbi:make-driver provider conn)))
      (migratum:provider-init provider)
      (migratum:driver-init driver)
      (unwind-protect (migratum:display-pending driver)
        (migratum:provider-shutdown provider)
        (migratum:driver-shutdown driver)))))

(defun new-migration (description &key (kind :sql) (open-emacs-p t))
  (anypool:with-connection (conn *dbi-pool*)
    (let* ((migration-id (migratum:make-migration-id))
           (provider (migratum.provider.local-path:make-provider
                      (list (asdf:system-relative-pathname "booker" "db/migrations/")))))
      (migratum:provider-init provider)
      ;; migratum requires a down migartion script, but we want only forward migrations
      ;; so we create a dummy
      (migratum:provider-create-migration :down kind provider migration-id description "-- DUMMY: We want forward migrations only!")
      (let ((path (namestring (migratum.provider.local-path:migration-up-script-path
                               (migratum:provider-create-migration :up kind provider migration-id description "")))))
        (if open-emacs-p
            (uiop:launch-program (list *emacsclient-exec* "-n" path))
            path)))))

;;; data api

(defmacro q ((&key (operation 'datafly:retrieve-all) transaction) &body body)
  (let ((gconn (gensym "gconn")))
    `(anypool:with-connection (,gconn *dbi-pool*)
       (let ((datafly:*connection* ,gconn))
         ,(if transaction
              `(dbi:with-transaction ,gconn
                 (,operation ,@body))
              `(,operation ,@body))))))

(defun string-empty-p (string)
  (check-type string string)
  (or (null string) (zerop (length string))))

(defun generate-db-id ()
  (anypool:with-connection (conn *dbi-pool*)
    (let ((replace-into (dbi:prepare conn "REPLACE INTO db_id_generator (val) VALUES ('a')"))
          (select-id (dbi:prepare conn "SELECT id FROM db_id_generator")))
      (dbi:with-transaction conn
        (dbi:execute replace-into)
        (getf (dbi:fetch (dbi:execute select-id))
              :|id|)))))

(defun all-bookmarks ()
  (q ()
    (select (:id :url :title)
      (from :bookmarks)
      (order-by (:desc :created_at)))))

(defun find-bookmark (id)
  (q (:operation datafly:retrieve-one)
    (select (:id :title :url)
      (from :bookmarks)
      (where (:= :id (typecase id
                       (string (parse-integer id))
                       (otherwise id)))))))

(defun find-bookmark-with-body (id)
  (q (:operation datafly:retrieve-one)
    (select (:id :title :url :body)
      (from :bookmarks)
      (where (:= :id (typecase id
                       (string (parse-integer id))
                       (otherwise id)))))))

(defun search-bookmarks (st)
  (anypool:with-connection (conn *dbi-pool*)
    (let ((query (dbi:prepare conn "SELECT id, url, title FROM bookmarks WHERE bookmarks MATCH ?")))
      (dbi:fetch-all (dbi:execute query (list st))))))

(defun create-bookmark (title url &optional body)
  (anypool:with-connection (conn *dbi-pool*)
    (let ((id (generate-db-id))
          (bookmark-title (if (string-empty-p title)
                              url
                              title))
          (insert-into  (dbi:prepare conn "
INSERT INTO bookmarks (id, title, url, body, created_at)
VALUES (?, ?, ?, ?, datetime('now'))")))
      (dbi:execute insert-into (list id bookmark-title url body))
      id)))

(defun update-bookmark (bookmark)
  (q (:operation datafly:execute)
    (update :bookmarks
      (set= :title (getf bookmark :title)
            :url (getf bookmark :url))
      (where (:= :id (getf bookmark :id))))))

(defun delete-bookmark (id)
  (q (:operation datafly:execute)
    (delete-from :bookmarks
      (where (:= :id (typecase id
                       (string (parse-integer id))
                       (t id)))))))
