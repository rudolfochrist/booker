;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:booker/test
  (:use :cl :booker :fiveam :fiveam-matchers))

(in-package #:booker/test)

(def-suite* :booker/test)

(test retrieve-bookmark-content-requires-url
  (signals booker::no-url-found
    (booker::retrieve-bookmark-content ""))
  (signals booker::no-url-found
    (booker::retrieve-bookmark-content nil)))

(test create-bookmark-with-url-for-missing-title
  (let* ((url "https://example-example.org")
         (id (booker/db:create-bookmark "" url))
         (title (getf (booker/db:find-bookmark id) :title)))
    (assert-that title (equal-to url))))
