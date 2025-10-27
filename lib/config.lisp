;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defun env-lookup (var &optional default)
  (or (uiop:getenvp var) default))

(defun last-pathname-component (&optional (path (uiop:getcwd)))
  (first (last (pathname-directory path))))

(defun secure-random-hex (&optional (n 16))
  (crypto:byte-array-to-hex-string
   (crypto:random-data n)))

(defun generate-secret ()
  (secure-random-hex 64))

(defclass config ()
  ((root :initform (uiop:getcwd)
         :initarg :root
         :accessor root-path
         :documentation "Root directory path.")
   (protect-against-forgery :initform t
                            :initarg :protect-agains-forgery
                            :accessor protect-against-forgery
                            :documentation "Protect against CSRF.")
   (forgery-protection-origin-check :initform t
                                    :initarg :forgery-protection-origin-check
                                    :accessor protect-against-forgery
                                    :documentation "Check origin for CSRF.")
   (env :initform (env-lookup "APP_ENV" "development")
        :initarg :env
        :accessor env
        :documentation "The current environment this app is running.")
   (name :initform (last-pathname-component)
         :initarg :name
         :accessor name
         :documentation "Application name.")
   (address :initform "127.0.0.1"
            :initarg :address
            :accessor address
            :documentation "Application address.")
   (port :initform 5000
         :initarg :port
         :accessor port
         :documentation "Application port.")
   (secret-key-base :initform (generate-secret)
                    :initarg :secret-key-base
                    :accessor secret-key-base
                    :documentation "Secret Key Base for encrypting sessions, etc.")
   (database-name :initform (format nil "~A_~A"
                                    (last-pathname-component)
                                    (env-lookup "APP_ENV" "development"))
                  :initarg :database-name
                  :accessor database-name)
   (database-user :initform (last-pathname-component)
                  :initarg :database-user
                  :accessor database-user)
   (database-host :initform :unix
                  :initarg :database-host
                  :accessor database-host)
   (database-port :initform 5432
                  :initarg :database-port
                  :accessor database-port)
   (pg-use-ssl :initform :try
               :initarg :pg-use-ssl
               :accessor pg-use-ssl)
   (access-log-destination :initform nil
                           :initarg :access-log-destination
                           :accessor access-log-destination)
   (message-log-destination :initform nil
                            :initarg :message-log-destination
                            :accessor message-log-destination))
  (:documentation "Application configurations"))


(defvar *config* (make-instance 'config)
  "Application configuration instance.")

(defun root (path)
  (merge-pathnames path (root-path *config*)))

(defun reload-config ()
  (setf *config* (make-instance 'config))
  (let ((config-file (make-pathname :defaults (root "config/")
                                    :name (env *config*)
                                    :type "lisp")))
    (format t "~&Loading ~A~%" config-file)
    (load config-file :if-does-not-exist nil)))

(defmacro maybe-update-config (place new-value)
  "Updates the configuration unless new-value is nil."
  `(setf ,place (or ,new-value ,place)))


