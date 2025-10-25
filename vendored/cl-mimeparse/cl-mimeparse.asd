(asdf:defsystem #:cl-mimeparse
  :description "Library for parsing MIME types, in the spirit of http://code.google.com/p/mimeparse/, with a Common Lisp flavor."
  :author "Nathan Froyd <froydnj@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :homepage "https://github.com/mmontone/cl-mimeparse"
  :serial t
  :components ((:file "package")
               (:file "mimeparse"))
  :depends-on (:cl-ppcre :parse-number))
