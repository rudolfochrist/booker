;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:booker/main
  (:use :cl)
  (:export
   #:main))

(in-package #:booker/main)

(defun handle-interrupt (signo info-sap context-sap)
  (declare (ignore signo info-sap context-sap))
  (booker:stop-application)
  (uiop:quit))

(defun reload (signo info-sap context-sap)
  (declare (ignore signo info-sap context-sap))
  (asdf:load-system "booker" :force t)
  (booker:reload-config))

(defun main (&optional args)
  (declare (ignore args))
  (sb-sys:enable-interrupt sb-unix:sigint #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sigterm #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sighup #'reload)
  (asdf:initialize-source-registry
   `(:source-registry
     (:tree ,(uiop:getcwd))
     :ignore-inherited-configuration))
  (booker:start-application t))

(push #'main sb-ext:*init-hooks*)


