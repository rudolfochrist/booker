;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defclass development-acceptor (hunchentoot:easy-acceptor hunchentoot-errors:errors-acceptor)
  ()
  (:documentation "Hunchentoot acceptor for development use. Has extended error reporting."))

(defvar *app* nil)

(defun initialize-application (&key reset name address port)
  (when (or (null *app*)
            reset)
    (reload-config)
    (setf *app* nil
          hunchentoot:*methods-for-post-parameters* (list :post :put :patch :delete)
          hunchentoot:*rewrite-for-session-urls* nil  ; use only cookies for sessions.
          hunchentoot:*session-secret* (secret-key-base *config*)
          hunchentoot:*show-lisp-errors-p* t)
    (setf *app* (make-instance (if (string= (env *config*) "development")
                                   'development-acceptor
                                   'hunchentoot:easy-acceptor)
                               :name (maybe-update-config (name *config*) name)
                               :address (maybe-update-config (address *config*) address)
                               :port (maybe-update-config (port *config*) port)
                               :document-root (root "public/")
                               :error-template-directory (when (string/= (env *config*) "development")
                                                           (root "public/"))
                               :access-log-destination (access-log-destination *config*)
                               :message-log-destination (message-log-destination *config*)))))


(defun start-application (&optional initialize)
  (when initialize
    (initialize-application))
  (when (null *app*)
    (error "Application not initialized!"))
  (hunchentoot:start *app*)
  (format t "~&Visit ~A://~A:~D~%"
          (if (hunchentoot:ssl-p *app*) "https" "http")
          (hunchentoot:acceptor-address *app*)
          (hunchentoot:acceptor-port *app*))
  (values (hunchentoot:acceptor-address *app*)
          (hunchentoot:acceptor-port *app*)))

(defun stop-application ()
  (when *app*
    (hunchentoot:stop *app* :soft t)
    (setf *app* nil)))

(defun application-running-p ()
  (and (not (null *app*))
       (ht:started-p *app*)))
