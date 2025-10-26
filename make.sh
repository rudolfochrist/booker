#!/bin/sh

LISP="${LISP:-sbcl}"
CLFLAGS="${CLFLAGS:---non-interactive --no-userinit}"

clean () {
    rm -f -- **/*.fasl
    rm -f booker.image
    exit 0
}

default () {
    "$LISP" $CLFLAGS --load build.lisp
}

"${@:-default}"
