;;;; package.lisp

(defpackage #:spinneret
  (:use #:cl)
  (:export #:with-html #:with-html-string #:html
           #:*html*
           #:*html-lang* #:*html-charset*
           #:*html-path*
           #:html-stream
           #:get-html-path
           #:do-elements
           #:deftag
           #:*unvalidated-attribute-prefixes*
           #:*boolean-attributes*
           #:*fill-column*
           #:html-length
           #:dynamic-tag
           #:*html-style*
           #:spinneret-error
           #:no-such-tag
           #:*suppress-inserted-spaces*
           #:interpret-html-tree
           #:escape-string
	   #:*always-quote*)
  (:import-from #:trivial-gray-streams
                #:fundamental-character-output-stream
                #:stream-write-char #:stream-write-string
                #:stream-terpri
                #:stream-fresh-line
                #:stream-finish-output
                #:stream-force-output
                #:stream-advance-to-column
                #:stream-start-line-p
                #:stream-line-column)
  (:import-from #:alexandria
                #:array-index
                #:clamp
                #:string-designator
                #:make-keyword
                #:parse-body #:parse-ordinary-lambda-list
                #:with-gensyms #:with-unique-names
                #:remove-from-plist
                #:starts-with-subseq
                #:when-let #:if-let
                #:assoc-value
                #:disjoin
                #:doplist
		#:hash-table-keys
                #:alist-hash-table
                #:once-only
                #:first-elt)
  (:import-from #:serapeum
                #:fmt #:eif #:econd
                #:define-do-macro #:defconst
                #:nlet #:nix #:assure
                #:find-keyword
                #:-> #:with-thunk
                #:and-let* #:op #:string-prefix-p
                #:memq
                #:string$=
                #:string^=
                #:escape
                #:defconst
                #:defconstructor
                #:string-replace-all
                #:local*
                #:fbind
                #:fbind*
                #:bound-value
                #:defmethods
                #:eval-if-constant
                #:parse-leading-keywords
                #:car+cdr
                #:mvlet*
                #:receive
                #:set-hash-table
                #:do-hash-table
                #:eval-always
                #:lret
                #:do-hash-table
                #:whitespacep)
  (:import-from #:cl-ppcre
                #:split)
  (:import-from #:trivia
                #:match)
  (:import-from #:global-vars
                #:define-global-parameter))

(defpackage #:spinneret-user
  (:use #:cl #:spinneret))
