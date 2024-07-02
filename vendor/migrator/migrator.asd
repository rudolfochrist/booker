;;; migrator.asd

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defsystem "migrator"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/migrator"
  :bug-tracker "https://github.com/rudolfochrist/migrator/issues"
  :source-control (:git "https://github.com/rudolfochrist/migrator.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop")
               "sqlite"
               "cl-ppcre"
               "verbose")
  :components ((:file "migrator"))
  :description "Simple declarative schema migration for SQLite."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.txt"))
  :in-order-to ((test-op (test-op "migrator/test"))))


(defsystem "migrator/test"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :description "Tests for migrator"
  :depends-on ((:require "uiop")
               "1am"
               "migrator"
               "verbose")
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (when (and (not (uiop:symbol-call :migrator/test :run-tests))
                               (uiop:getenv "NON_INTERACTIVE_TESTS"))
                      (uiop:quit 1))))


