;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:booker/db
  (:use :cl :booker :sxql)
  (:export
   #:migrate
   #:create-migration
   #:all-bookmarks
   #:generate-db-id
   #:save-bookmark
   #:find-bookmark
   #:update-bookmark
   #:delete-bookmark))

(in-package #:booker/db)

(defvar *dbi-pool*
  (anypool:make-pool :name "dbi-connections"
                     :connector (lambda ()
                                  (dbi:connect :sqlite3 :database-name (booker:root "db/data.db")))
                     :disconnector #'dbi:disconnect
                     :ping #'dbi:ping))

(defun migrate ()
  (anypool:with-connection (conn *dbi-pool*)
    (let* ((provider (migratum.provider.local-path:make-provider (list (root "db/migrations/"))))
           (driver (migratum.driver.dbi:make-driver provider conn)))
      (migratum:provider-init provider)
      (migratum:driver-init driver)
      (migratum:apply-pending driver))))

(defun create-migration (name)
  (let ((provider (migratum.provider.local-path:make-provider (list (root "db/migrations/"))))
        (id (migratum:make-migration-id)))
    (migratum:provider-init provider)
    (migratum:provider-create-migration :up :sql provider id name "")
    (migratum:provider-create-migration :down :sql provider id name "")))


;;; data api

(defmacro q ((&key (operation 'datafly:retrieve-all) transaction) &body body)
  (let ((gconn (gensym "gconn")))
    `(anypool:with-connection (,gconn *dbi-pool*)
       (let ((datafly:*connection* ,gconn))
         ,(if transaction
              `(dbi:with-transaction ,gconn
                 (,operation ,@body))
              `(,operation ,@body))))))

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
      (from :bookmarks))))

(defun find-bookmark (id)
  (q (:operation datafly:retrieve-one)
    (select (:id :title :url)
      (from :bookmarks)
      (where (:= :id (typecase id
                       (string (parse-integer id))
                       (otherwise id)))))))

(defun create-bookmark (title url body)
  (let ((id (generate-db-id)))
    (q (:operation datafly:execute)
      (insert-into :bookmarks
        (set= :id id
              :title title
              :url url
              :body body)))))

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
