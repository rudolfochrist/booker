;;; Copyright 2008 Nathan Froyd
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation files
;;; (the "Software"), to deal in the Software without restriction,
;;; including without limitation the rights to use, copy, modify, merge,
;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;; and to permit persons to whom the Software is furnished to do so,
;;; subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;;; SOFTWARE.

(defpackage :mimeparse-tests
  (:use :cl))

(in-package :mimeparse-tests)

;;; rfc2616 example

(defvar *accept*
  "text/*;q=0.3, text/html;q=0.7, text/html;level=1, text/html;level=2;q=0.4, */*;q=0.5")

(defvar *rfc2616-tests*
  '(("text/html;level=1" 1)
    ("text/html" 0.7)
    ("text/plain" 0.3)
    ("image/jpeg" 0.5)
    ("text/html;level=2" 0.4)
    ("text/html;level=3" 0.7)))

(rtest:deftest rfc2616
  (loop for (mime-type expected-q) in *rfc2616-tests*
        do (let ((q (mimeparse:quality mime-type *accept*)))
             (unless (eql q expected-q)
               (error "Expected ~A for mime-type ~A, got ~A"
                      expected-q mime-type q)))
        finally (return t))
  t)

;; best match testing

(defvar *best-match-tests*
  (list (list "application/xbel+xml" "application/xml")
        '(("application/xbel+xml" "application/xbel+xml") ; direct match
          ("application/xbel+xml; q=1" "application/xbel+xml") ; direct match with q
          ("application/xml; q=1" "application/xml")           ; direct match of #2 with q
          ("application/*; q=1" "application/xml")             ; subtype wildcard
          ("*/*" "application/xml"))                           ; type wildcard
        (list "application/xbel+xml" "text/xml")
        '(("text/*;q=0.5,*/*; q=0.1" "text/xml") ; type versus lower-weighted subtype
          ("text/html,application/atom+xml; q=0.9" "")) ; match failure
        (list "application/json" "text/html")
        '(("application/json, text/javascript, */*" "application/json")
          ("application/json, text/html;q=0.9" "application/json"))
        (list "image/*" "application/xml")
        '(("image/png" "image/*")       ; type wildcard
          ("image/*" "image/*")))) ; wildcard for requested and supported

(rtest:deftest best-match
  (loop for (supported-types tests . rest) on *best-match-tests* by #'cddr
        do (loop for (requested expected) in tests
                 do (let ((actual (mimeparse:best-match supported-types requested)))
                      (unless (string= actual expected)
                        (error "Requested ~A from supported types ~A and received ~A"
                               requested supported-types actual))))
        finally (return t))
  t)
