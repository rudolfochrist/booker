;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

;;; SPDX-License-Identifier: MPL-2.0

(defpackage #:sefil/doc
  (:use :cl))

(in-package #:sefil/doc)

(sb-texinfo:document-packages '(:sefil) "sefil"
                              :output-file (asdf:system-relative-pathname :sefil "doc/sefil.texi")
                              :write-backmatter nil
                              :write-menu nil
                              :exclude-node t)
