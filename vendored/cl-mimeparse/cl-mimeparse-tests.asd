(asdf:defsystem #:cl-mimeparse-tests
  :description "Library for parsing MIME types, in the spirit of http://code.google.com/p/mimeparse/, with a Common Lisp flavor."
  :author "Nathan Froyd <froydnj@gmail.com>"
  :license  "MIT"
  :version "0.0.1"
  :serial t
  :components ((:file "tests"))
  :depends-on (:cl-mimeparse :rt))
