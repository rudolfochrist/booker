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
               "spinneret"
               "cl-dbi"
               "datafly"
               "anypool"
               "cl-migratum"
               "cl-migratum.provider.local-path"
               "cl-migratum.driver.dbi"
               "ironclad"
               "myway"
               "hunchentoot-errors"
               "com.inuoe.jzon"
               "plump"
               "dexador"
               "access")
  :components ((:file "package")
               (:module "lib"
                :components ((:file "config")
                             (:file "hunchentoot-ext")
                             (:file "routing")
                             (:file "server")))
               (:module "app"
                :components ((:file "db")
                             (:file "views")
                             (:file "controllers"))))
  :description "A bookmark managing application."
  :long-description
  #.(uiop:read-file-string
     (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op "booker/test"))))


