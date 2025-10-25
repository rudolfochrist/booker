;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; SPDX-License-Identifier: MPL-2.0

(defsystem "sefil"
  :author "Sebastian Christ <rudolfo.christ@proton.me>"
  :mailto "rudolfo.christ@proton.me"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/sefil"
  :bug-tracker "https://github.com/rudolfochrist/sefil/issues"
  :source-control (:git "https://github.com/rudolfochrist/sefil.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop")
               "cl-ppcre"
               "bordeaux-threads"
               "ironclad/cipher/aes"
               "ironclad/aead/gcm"
               "ironclad/kdf/password-hash"
               "osicat"
               "cl-conspack"
               "cl-base64")
  :serial t
  :components ((:module "lib"
                :components ((:file "sefil"))))
  :description "sefil -- SEcure FILes"
  :in-order-to ((test-op (test-op "sefil/test"))))


(defsystem "sefil/test"
  :description "Tests for sefil"
  :depends-on ((:require "uiop")
               "fiveam"
               "sefil")
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :fiveam :run! :sefil)
                      #+(not (or :swank :slynk))
                      (uiop:quit 1))))


(defsystem "sefil/cli"
  :build-operation program-op
  :build-pathname "sefil"
  :entry-point "sefil/main:main"
  :description "sefil Executable"
  :depends-on ("sefil"
               "clingon")
  :components ((:file "main")))

#+sb-core-compression
(defmethod asdf:perform ((op asdf:image-op) (c (eql (asdf:find-system "sefil/cli"))))
  (uiop:dump-image (asdf:output-file op c) :executable t :compression t))

(defsystem "sefil/doc"
  :depends-on ("sefil"
               "sb-texinfo")
  :components ((:module "doc"
                :components ((:file "documentation")))))
