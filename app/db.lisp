;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:booker/db
  (:use :cl :booker)
  (:export
   *db*))

(in-package #:booker/db)

(defvar *db* (jasql.sqlite:make-handle :path "db/data.db"))

(jasql:load-sql "sql/queries.sql" :system "booker")
