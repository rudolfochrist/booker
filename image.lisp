;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:booker/image
  (:use :cl))

(in-package #:booker/image)

(defun handle-interrupt (signo info-sap context-sap)
  (declare (ignore signo info-sap context-sap))
  (booker:stop-application)
  (uiop:quit))

(defun init (&optional args)
  (declare (ignore args))
  (sb-sys:enable-interrupt sb-unix:sigint #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sigterm #'handle-interrupt)
  (booker:start-application t))

(push #'init sb-ext:*init-hooks*)


