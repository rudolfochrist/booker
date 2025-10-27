;;; SPDX-License-Identifier: MPL-2.0
;;;
;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(in-package #:booker)

(defmacro csrf-meta-tags ()
  `(spinneret:with-html-string
     (:meta :name "csrf-param" :content "authenticity_token")
     (:meta :name "csrf-token" :content (form-authenticity-token))))


(defmacro hidden-authenticity-token ()
  `(spinneret:with-html-string
     (:input :type "hidden"
             :name "authenticity_token"
             :value (form-authenticity-token)
             :autocomplete "off")))

