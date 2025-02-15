;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defvar *db* nil)

(defgeneric initialize-database ()
  (:documentation "Application specific database initialization."))

(defmethod initialize-database :around ()
  (setf *db* (call-next-method)))

(defmethod initialize-database ()
  (jasql.postgres:make-handle
   :database (database-name *config*)
   :username (database-user *config*)
   :host (database-host *config*)
   :port (database-port *config*)
   :use-ssl (pg-use-ssl *config*)))

(defpackage #:booker/db
  (:use :cl))

(in-package #:booker/db)

(jasql:load-sql "sql/queries.sql" :system "booker")
