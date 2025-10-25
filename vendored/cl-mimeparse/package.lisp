(defpackage :mimeparse
  (:use :cl)
  ;; Types
  (:export #:media-range)
  ;; Functions
  (:export #:parse-media-range
           #:quality
           #:best-match))
