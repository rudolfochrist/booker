;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(djula:add-template-directory (asdf:system-relative-pathname "booker" "app/views/"))

(defparameter +up.html+ (djula:compile-template* "up.html"))

(defparameter +bookmarks-index.html+ (djula:compile-template* "bookmarks-index.html"))
