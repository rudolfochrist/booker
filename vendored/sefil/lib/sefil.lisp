;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; SPDX-License-Identifier: MPL-2.0

(defpackage #:sefil
  (:use :cl)
  (:export
   #:generate-key
   #:add-key-file
   #:read-key-file
   #:key
   #:encrypt
   #:decrypt
   #:encrypt-file
   #:decrypt-file
   #:create-empty-encrypted-file
   #:*key-path*)
  (:documentation "sefil base package."))

(in-package #:sefil)

(defvar *key-path* (make-pathname :defaults (uiop:getcwd)
                                  :name "master"
                                  :type "key")
  "Pathname to the key file to use. Defaults to $PWD/master.key")

(define-condition no-key-error (error)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
             (format stream "Key file ~A does not exist. Please generate a key file." *key-path*)))
  (:documentation "Error condition when a key file cannot be found."))

(defun generate-key ()
  "AES-128 -> key length 16 bytes"
  (crypto:byte-array-to-hex-string (crypto:random-data 16)))


(defun add-key-file (&key path key)
  "Add a keyfile"
  (let ((key-file (or path *key-path*)))
    (unless (uiop:file-exists-p key-file)
      (with-open-file (out key-file
                           :direction :output
                           :if-exists :error
                           :if-does-not-exist :create)
        (princ (or key (generate-key)) out))
      (setf (osicat:file-permissions key-file) (list :user-read :user-write)))
    key-file))


(defun read-key-file (pathname)
  "Read the key file from PATHNAME."
  (when (uiop:file-exists-p pathname)
    (uiop:read-file-line pathname)))

(defun key (&optional keyfile-path)
  "Retrive the encryption/decryption key.

The key found in the SEFIL_MASTER_KEY environment variable takes
precedence if set. "
  (or (uiop:getenv "SEFIL_MASTER_KEY")
      (read-key-file (or keyfile-path *key-path*))
      (error 'no-key-error)))


(defun encrypt (data &optional secret)
  "Encrypt DATA."
  (let* ((key (crypto:hex-string-to-byte-array (or secret (key))))
         (iv (crypto:make-random-salt))
         (cipher (crypto:make-authenticated-encryption-mode
                  :gcm
                  :cipher-name :aes
                  :key key
                  :initialization-vector iv)))
    (crypto:process-associated-data cipher (babel:string-to-octets ""))
    (format nil "~{~A~^--~}"
            (mapcar #'base64:usb8-array-to-base64-string
                    (list (crypto:encrypt-message cipher (conspack:encode data))
                          iv
                          (crypto:produce-tag cipher))))))

(defun decrypt (data &optional secret)
  "Decrypt DATA."
  (destructuring-bind (encrypted-data iv tag)
      (mapcar #'base64:base64-string-to-usb8-array (ppcre:split "--" data))
    (let* ((key (crypto:hex-string-to-byte-array (or secret (key))))
           (cipher (crypto:make-authenticated-encryption-mode
                    :gcm
                    :tag tag
                    :cipher-name :aes
                    :key key
                    :initialization-vector iv)))
      (crypto:process-associated-data cipher (babel:string-to-octets ""))
      (conspack:decode (crypto:decrypt-message cipher encrypted-data)))))

(defun write-file-string (file string &key (if-exists :supersede))
  (with-open-file (stream file
                          :direction :output
                          :if-exists if-exists)
    (write-string string stream)
    (force-output stream)))

(defun encrypt-file (path &key secret output)
  "Encrypt file at PATH."
  (when (uiop:file-exists-p path)
    (let ((k (or secret (key))))
      (etypecase output
        (stream
         (write-string (encrypt (uiop:read-file-string path) k) output)
         (force-output output))
        (pathname
         (write-file-string output
                            (encrypt (uiop:read-file-string path) k)))
        (string
         (encrypt-file path :secret secret :output (pathname output)))
        (null
         (encrypt (uiop:read-file-string path) k))))))

(defun decrypt-file (path &key secret output)
  "Decrypt file at PATH."
  (when (uiop:file-exists-p path)
    (let ((k (or secret (key))))
      (etypecase output
        (stream
         (write-string (decrypt (uiop:read-file-string path) k) output)
         (force-output output))
        (pathname
         (write-file-string output (decrypt (uiop:read-file-string path) k)))
        (string
         (decrypt-file path :secret secret :output (pathname output)))
        (null
         (decrypt (uiop:read-file-string path) k))))))

(defun create-empty-encrypted-file (path &optional key)
  "Create an empty encrypted file at PATH."
  (let ((k (or key (key))))
    (write-file-string path (encrypt "" k))))
