;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; SPDX-License-Identifier: MPL-2.0

(defpackage #:sefil/test
  (:use :cl :fiveam))

(in-package #:sefil/test)

(def-suite* :sefil)

(test encrypt-decrypt-idempotence
  (let ((key (sefil:generate-key)))
    (is (string-equal "no more secrets"
                      (sefil:decrypt (sefil:encrypt "no more secrets" key) key)))))



