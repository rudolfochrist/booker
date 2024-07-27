;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defun maybe-string-coerce (object)
  (typecase object
    (symbol (string-downcase (string object)))
    (t object)))

(defun string-symbol-equal (object1 object2)
  (string= (maybe-string-coerce object1)
           (maybe-string-coerce object2)))

(defun keywordize (object)
  (typecase object
    (keyword object)
    (string (alexandria:make-keyword (string-upcase object)))))


(defun match (method path)
  (let ((route (make-instance 'myway:route :url path :method method)))
    (lambda (request)
      (multiple-value-bind (matchp vars)
          (myway:match-route route
                             (keywordize (or (ht:post-parameter "_method")
                                             (hunchentoot:request-method request)))
                             (hunchentoot:script-name request))
        (when matchp
          (let ((path-params (alexandria:plist-alist vars))
                (query-params (hunchentoot:get-parameters request))
                (request-params (hunchentoot:post-parameters request)))
            (setf (hunchentoot::aux-data request)
                  (union (union request-params
                                query-params
                                :key #'car
                                :test #'string-equal)
                         path-params
                         :key #'car
                         :test #'string-equal)))
          t)))))

(defun params (name &optional (request hunchentoot:*request*))
  (cdr (assoc name (hunchentoot::aux-data request) :test #'string-equal)))

(defun status (code)
  (setf (hunchentoot:return-code*) code)
  (hunchentoot:send-headers))

(defun redirect (target &optional (code hunchentoot:+http-see-other+))
  (hunchentoot:redirect target :code code :add-session-id nil))
