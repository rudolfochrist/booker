;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:hunchentoot)

(def-http-return-code +http-unprocessable-content+ 422 "Unprocessable Content")
(export '+http-unprocessable-content+)

(defmethod session-cookie-name ((acceptor easy-acceptor))
  (format nil "_~A_session" booker::*name*))
(in-package #:chunga)

(defun as-keyword (string &key (destructivep t))
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Might destructively modify STRING if DESTRUCTIVEP is true which
is the default.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (declare (ignore destructivep))
  (gethash string +string-to-keyword-hash+))
