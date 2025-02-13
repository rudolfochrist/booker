;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defparameter *db* (jasql.postgres:make-handle
                    :database (env "PG_DATABASE_NAME"
                                   (format nil "booker_~A" *env*))
                    :username (env "PG_USER" "booker")
                    :host (env "PG_HOST" :unix)
                    :port (or (and (env "PG_PORT")
                                   (parse-integer (env "PG_PORT")))
                              5432)
                    :use-ssl (if (env "PG_SSL")
                                 :try
                                 :no)))

(defpackage #:booker/db
  (:use :cl))

(in-package #:booker/db)

(jasql:load-sql "sql/queries.sql" :system "booker")
