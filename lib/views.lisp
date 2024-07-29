;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(djula:def-tag-compiler :csrf-meta-tags ()
  (lambda (stream)
    (let ((csrf-token (form-authenticity-token)))
      (format stream "~
<meta name=\"csrf-param\" content=\"authenticity_token\" />
~8@T<meta name=\"csrf-token\" content=\"~A\" />"
              csrf-token))))

(djula:def-tag-compiler :form-authenticity-token ()
  (lambda (stream)
    (let ((auth-token (form-authenticity-token)))
      (format stream "~
<input type=\"hidden\" name=\"authenticity_token\" value=\"~A\" autocomplete=\"off\"/>"
              auth-token))))
