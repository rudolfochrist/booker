;;; t/tests.lisp

;;; This Source Code Form is subject to the terms of the Mozilla Public
;;; License, v. 2.0. If a copy of the MPL was not distributed with this
;;; file, You can obtain one at http://mozilla.org/MPL/2.0/.

(defpackage #:migrator/test
  (:use :cl :1am)
  (:local-nicknames
   (#:v :org.shirakumo.verbose)))

(in-package #:migrator/test)

(defvar *tests* nil)

(defmacro deftest (name &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (test ,name
       ,@body)
     (pushnew ',name *tests*)
     ',name))

(defun run-tests (&optional (tests *tests*))
  (1am:run tests))

;;; Test cases


(deftest test-schema-migrations
  (sqlite:with-open-database (db ":memory:")
    ;; setup
    (migrator::execute-script db (asdf:system-relative-pathname "migrator" "t/mig-a/schema.sql"))
    (is (equal (sqlite:execute-to-list db "select node_id, something_else from Node")
               '(("123" "old"))))
    ;; migrate
    (v:with-muffled-logging (:migrator)
      (migrator:migrate db (asdf:system-relative-pathname "migrator" "t/mig-b/") :allow-deletions t))
    ;; verify tables
    (is (equal (sqlite:execute-to-list db "select name from sqlite_master where type = 'table' order by name")
               '(("Foo") ("Node"))))
    ;; verify indexes
    (is (equal (sqlite:execute-to-list db "select name from sqlite_master where type = 'index' and name not like \"sqlit_%\" order by name")
               '(("Foo_foo_id") ("Node_node_id"))))
    ;; verify pragma
    (is (= (sqlite:execute-single db "pragma user_version")
           1))
    ;; verify columns
    (is (equal (sqlite:execute-to-list db "select name, type  from pragma_table_info('Node') order by name")
               '(("created_at" "DATETIME") ("node_id" "TEXT") ("node_oid" "INTEGER") ("something_else" "TEXT"))))
    ;; verify data not deleted
    (is (equal (sqlite:execute-to-list db "select node_id, something_else from Node")
               '(("123" "new"))))))
