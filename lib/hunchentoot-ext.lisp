;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:hunchentoot)

(def-http-return-code +http-unprocessable-content+ 422 "Unprocessable Content")
(export '+http-unprocessable-content+)

(defmethod session-cookie-name ((acceptor easy-acceptor))
  (format nil "_~A_session" booker::*name*))

(defmethod session-created ((acceptor easy-acceptor) session)
  ;; Set SameSite=Lax for session cookies
  (declare (ignore session))
  (let ((cookie (cdr (assoc (session-cookie-name acceptor) (cookies-out*) :test #'string=))))
    (setf (cookie-same-site cookie) "Lax")))

(in-package #:chunga)

(eval-when (:compile-toplevel :load-toplevel)
  (defvar *string-to-keyword-hash* (alexandria:copy-hash-table +string-to-keyword-hash+))

  (defvar +more-known-words+
    '("Cookie"
      "Forwarded"
      "Origin"
      "Priority"
      "Sec-Fetch-Dest"
      "Sec-Fetch-Mode"
      "Sec-Fetch-Site"
      "Sec-Fetch-User"
      "Upgrade-Insecure-Requests"
      "X-Forwarded-For"
      "X-Forwarded-Host"
      "X-Forwarded-Proto"))

  (dolist (word +more-known-words+)
    (setf (gethash word *string-to-keyword-hash*) (make-keyword word nil))))

(defun as-keyword (string &key (destructivep t))
  "Converts the string STRING to a keyword where all characters are
uppercase or lowercase, taking into account the current readtable
case.  Might destructively modify STRING if DESTRUCTIVEP is true which
is the default.  \"Knows\" several HTTP header names and methods and
is optimized to not call INTERN for these."
  (declare (ignore destructivep))
  (gethash string *string-to-keyword-hash*))
