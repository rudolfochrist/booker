;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; SPDX-License-Identifier: MPL-2.0

(defpackage #:sefil/main
  (:use :cl)
  (:export
   #:main))

(in-package #:sefil/main)

(defparameter *system* (asdf:find-system "sefil"))

(defun toplevel-options ()
  (list
   (clingon:make-option
    :string
    :description "secret key"
    :short-name #\k
    :long-name "key"
    :env-vars '("SEFIL_MASTER_KEY")
    :key :key)
   (clingon:make-option
    :filepath
    :description "use key file"
    :long-name "key-file"
    :key :keyfile)))

(defun keygen-command ()
  (clingon:make-command
   :name "keygen"
   :description "Generate a new key."
   :handler (lambda (cmd)
              (declare (ignore cmd))
              (sefil:add-key-file))))

(defun show-command ()
  (clingon:make-command
   :name "show"
   :description "Decrypt file and show its content."
   :usage "FILENAME"
   :handler (lambda (cmd)
              (let ((args (clingon:command-arguments cmd))
                    (secret (or (clingon:getopt cmd :key)
                                (clingon:getopt cmd :keyfile)
                                (sefil:key))))
                (if (and (> (length args) 0)
                         (uiop:file-exists-p (first args)))
                    (format t "~&~A~%" (sefil:decrypt (uiop:read-file-string (first args)) secret))
                    (error "File not exist."))))))

(defun edit-handler (cmd)
  (let ((args (clingon:command-arguments cmd))
        (secret (or (clingon:getopt cmd :key)
                    (clingon:getopt cmd :keyfile)
                    (sefil:key))))
    (when (<= (length args) 0)
      (error "File missing!"))
    (let ((file (first args)))
      (if (uiop:file-exists-p file)
          (uiop:with-temporary-file (:stream out
                                     :pathname tmpfile
                                     :direction :output)
            (sefil:decrypt-file file :secret secret :output out)
            (uiop:run-program (append (or (uiop:split-string (uiop:getenvp "EDITOR"))
                                          (list "vim"))
                                      (list (namestring tmpfile)))
                              :force-shell t
                              :output :interactive
                              :input :interactive)
            (sefil:encrypt-file tmpfile :secret secret :output file))
          (progn
            (sefil:create-empty-encrypted-file file)
            (edit-handler cmd))))))

(defun edit-command ()
  (clingon:make-command
   :name "edit"
   :description "Edit an encrypted file."
   :usage "FILENAME"
   :handler #'edit-handler))

(defun toplevel-command ()
  (clingon:make-command
   :name "sefil"
   :description "SEcure FILes"
   :version (asdf:component-version *system*)
   :license (asdf:system-licence *system*)
   :authors (remove-if #'null (list (asdf:system-author *system*)
                                    (asdf:system-maintainer *system*)))
   :options (toplevel-options)
   :sub-commands (list
                  (keygen-command)
                  (show-command)
                  (edit-command))
   :handler (lambda (cmd) (clingon:print-usage cmd t))))

(defun run ()
  (let ((app (toplevel-command)))
    (clingon:run app)))

(defun handle-interrupt (signo info-sap context-sap)
  (declare (ignore info-sap context-sap))
  ;; cleanup/finalize
  (uiop:quit (if (eq signo sb-unix:sigint)
                 130
                 0)))


(defun main (&optional args)
  (declare (ignorable args))
  (sb-sys:enable-interrupt sb-unix:sigint #'handle-interrupt)
  (sb-sys:enable-interrupt sb-unix:sigterm #'handle-interrupt)
  ;; run program
  (run))
