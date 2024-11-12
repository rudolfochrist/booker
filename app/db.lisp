;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defparameter *db* (jasql.postgres:make-handle
                    :database (or (uiop:getenvp "DATABASE_NAME")
                                  (format nil "booker_~A" *env*))
                    :username (or (uiop:getenvp "DATABASE_USERNAME")
                                  "booker")
                    :host (or (uiop:getenvp "DATABASE_HOST")
                              :unix)
                    :port (or (and (uiop:getenvp "DATABASE_PORT")
                                   (parse-integer (uiop:getenvp "DATABASE_PORT")))
                              5432)))

(defpackage #:booker/db
  (:use :cl))

(in-package #:booker/db)

(jasql:load-sql "sql/queries.sql" :system "booker")
