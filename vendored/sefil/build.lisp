;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; SPDX-License-Identifier: MPL-2.0

(require 'asdf)
(require 'uiop)
#+sbcl (require 'sb-aclrepl)

(in-package :cl-user)

(asdf:initialize-source-registry
 '(:source-registry
   (:tree (:here))
   :ignore-inherited-configuration))

(declaim (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 3)))
(asdf:make "sefil/cli")

(uiop:quit)
