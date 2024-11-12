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

(defun main (&optional args)
  (declare (ignore args))
  (sb-sys:enable-interrupt sb-unix:sigint #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sigterm #'handle-interrupt)
  (booker:start-application t)
  #+enable-executable
  (dolist (th (sb-thread:list-all-threads))
    (when (uiop:string-prefix-p "hunchentoot-listener" (sb-thread:thread-name th))
      (sb-thread:join-thread th))))

#+enable-image
(push #'main sb-ext:*init-hooks*)


