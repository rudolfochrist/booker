#!/bin/bash

sbcl --non-interactive --no-userinit --load init.lisp \
     --eval '(asdf:test-system "booker")' \
     --eval '(uiop:quit)'
