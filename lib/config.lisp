;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;;; root
  (defvar *root* (uiop:getcwd))
  (defun root (path)
    (merge-pathnames path *root*))

  (defun load-dotenv ()
    (let ((env-file (root ".env")))
      (when (probe-file env-file)
        (with-open-file (in env-file)
          (loop for line = (read-line in nil nil)
                while line
                if (> (length line) 0)
                  do (let ((pos (position #\= line :test #'char=)))
                       (when pos
                         (setf (uiop:getenv (subseq line 0 pos))
                               (subseq line (1+ pos)))))
                finally (return t))))))
  (load-dotenv))

;;; config variables
(defvar *protect-against-forgery* t)
(defvar *forgery-protection-origin-check* t)
(defvar *env* (or (uiop:getenvp "APP_ENV") "development"))
(defvar *name* (first (last (pathname-directory (uiop:getcwd)))))
(defvar *port* (or (uiop:getenvp "APP_PORT") 5000))
(defvar *config-path* (root "config/"))

;;; unbound -> user is forced to set theses in env-config file
(defvar *secret-key-base* (or (uiop:getenv "SECRET_KEY_BASE")
                              (error "Secret Key Base is missing! ~%Please set SECRET_KEY_BASE either as environment variable or in .env file.")))

;;; helpers
(defun load-config (&optional env)
  (uiop:if-let ((config (probe-file (make-pathname :defaults *config-path*
                                                   :name (or env *env*)
                                                   :type "lisp"))))
    (progn
      (format t "~&Loading config: ~A~%" config)
      (load config))))

(defun secure-random-hex (&optional (n 16))
  (crypto:byte-array-to-hex-string
   (crypto:random-data n)))

(defun generate-secret ()
  (secure-random-hex 64))

