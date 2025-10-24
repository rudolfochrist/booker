;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defsystem "booker"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :homepage "https://github.com/rudolfochrist/booker"
  :bug-tracker "https://github.com/rudolfochrist/booker/issues"
  :source-control (:git "https://github.com/rudolfochrist/booker.git")
  :version (:read-file-line "version")
  :depends-on ((:require "uiop")
               "alexandria"
               "hunchentoot"
               "postmodern"
               "ironclad"
               "myway"
               "hunchentoot-errors"
               "com.inuoe.jzon"
               "plump"
               "dexador"
               "spinneret"
               "sefil")
  :components ((:file "package")
               (:module "lib"
                :components ((:file "config")
                             (:file "hunchentoot-ext")
                             (:file "csrf")
                             (:file "routing")
                             (:file "views")
                             (:file "server")))
               (:module "sql"
                :components ((:static-file "queries.sql")))
               (:module "app"
                :components ((:file "db")
                             (:file "views")
                             (:file "controllers"))))
  :description "A bookmark managing application."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "booker/test"))))

(defsystem "booker/test"
  :author "Sebastian Christ <rudolfo.christ@pm.me>"
  :maintainer "Sebastian Christ <rudolfo.christ@pm.me>"
  :mailto "rudolfo.christ@pm.me"
  :license "MPL-2.0"
  :description "Tests for booker"
  :depends-on ((:require "uiop")
               "fiveam"
               "fiveam-matchers"
               "booker")
  :pathname "t/"
  :components ((:file "tests"))
  :perform (test-op (op c)
                    (unless (uiop:symbol-call :fiveam :run! :booker/test)
                      #+(not (or :swank :slynk))
                      (uiop:quit 1))))

(defsystem "booker/image"
  :build-operation image-op
  :build-pathname "booker"
  :description "Booker App Web Image"
  :depends-on ("booker")
  :components ((:file "main")))

