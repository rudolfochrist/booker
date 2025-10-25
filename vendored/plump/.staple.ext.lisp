(asdf:load-system :staple-markdown)
(defmethod staple:subsystems ((s (eql (asdf:find-system :plump)))) ())
