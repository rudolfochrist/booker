;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

 ;;; root
(defvar *root* (uiop:getcwd))
(defun root (path)
  (merge-pathnames path *root*))


(defmacro env (var &optional default)
  `(or (uiop:getenvp ,var) ,default))


(defun determine-server-address ()
  (uiop:if-let ((address (uiop:getenvp "APP_ADDRESS")))
    (if (uiop:string-suffix-p address ".sock")
        ;; unix domain socket
        (merge-pathnames address)
        ;; network
        (usocket:get-host-by-name address))
    "127.0.0.1"))

;;; config variables
(defparameter *protect-against-forgery* t)
(defparameter *forgery-protection-origin-check* t)
(defparameter *env* (env "APP_ENV" "development"))
(defparameter *name* (first (last (pathname-directory (uiop:getcwd)))))
(defparameter *address* (determine-server-address))
(defparameter *port* (or (and (env "APP_PORT")
                              (parse-integer (env "APP_PORT")))
                         5000))

(defparameter *secret-key-base* (env "SECRET_KEY_BASE"
                                     (error "Secret Key Base is missing! ~%Please set SECRET_KEY_BASE either~
as environment variable or in .env file.")))

(defun secure-random-hex (&optional (n 16))
  (crypto:byte-array-to-hex-string
   (crypto:random-data n)))

(defun generate-secret ()
  (secure-random-hex 64))

