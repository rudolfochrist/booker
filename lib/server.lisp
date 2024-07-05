;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defclass development-acceptor (hunchentoot:easy-acceptor hunchentoot-errors:errors-acceptor)
  ()
  (:documentation "Hunchentoot acceptor for development use. Has extended error reporting."))

(defvar *app* nil)

(defun initialize-application (&key (name *name*) (address *address*) (port *port*) reset)
  (when (or (null *app*)
            reset)
    (setf *app* nil
          *env* (or (uiop:getenvp "BOOKER_ENV") *env*))
    (load-config)
    (setf hunchentoot:*methods-for-post-parameters* (list :post :put :patch :delete)
          hunchentoot:*rewrite-for-session-urls* nil  ; use only cookies for sessions.
          hunchentoot:*session-secret* *secret-key-base*
          hunchentoot:*show-lisp-errors-p* t)
    (setf *app* (make-instance (if (string= *env* "development")
                                   'development-acceptor
                                   'hunchentoot:easy-acceptor)
                               :name name
                               :address address
                               :port port
                               :document-root (root "public/")
                               :error-template-directory (when (string/= *env* "development")
                                                           (root "public/"))))))


(defun start-application (&optional initialize)
  (when initialize
    (initialize-application))
  (when (null *app*)
    (error "Acceptor not initialized!"))
  (hunchentoot:start *app*)
  (format t "Visit ~A://~A:~D"
          (if (hunchentoot:ssl-p *app*) "https" "http")
          (hunchentoot:acceptor-address *app*)
          (hunchentoot:acceptor-port *app*))
  (values (hunchentoot:acceptor-address *app*)
          (hunchentoot:acceptor-port *app*)))

(defun stop-application ()
  (when *app*
    (hunchentoot:stop *app* :soft t)
    (setf *app* nil)))

(defun application-runnning-p ()
  (and (not (null *app*))
       (ht:started-p *app*)))
