;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(require 'asdf)
(require 'uiop)
#+sbcl (require 'sb-aclrepl)

(asdf:initialize-source-registry
 '(:source-registry
   (:tree (:here))
   :ignore-inherited-configuration))

(declaim (optimize (speed 3) (safety 1) (debug 1) (compilation-speed 3)))
(asdf:make "booker/image" :force t)
(gc :full t)

(uiop:quit)
