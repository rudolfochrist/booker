;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defvar *db* (jasql.sqlite:make-handle :path "db/data.db"))

(defpackage #:booker/db
  (:use :cl))

(in-package #:booker/db)

(jasql:load-sql "sql/queries.sql" :system "booker")
