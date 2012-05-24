;;; -*- mode: lisp -*-

(cl:defpackage #:trees-system
  (:use #:cl #:asdf))
(cl:in-package #:trees-system)

(asdf:defsystem :trees
  :version "0.11"
  :author "Nathan Froyd <froydnj@gmail.com>"
  :maintainer "Nathan Froyd <froydnj@gmail.com>"
  :description "A library for binary trees in normal and balanced flavors"
  :license "BSD style"
  :components ((:file "package")
               (:file "types" :depends-on ("package"))
               (:file "generics" :depends-on ("types"))
               (:file "print" :depends-on ("generics" "types"))
               (:file "binary-trees" :depends-on ("generics" "types"))
               (:file "red-black-trees" :depends-on ("types" "binary-trees"))
               (:file "avl-trees" :depends-on ("types" "binary-trees"))
               (:file "aa-trees" :depends-on ("types" "binary-trees"))
               (:file "interval-trees" :depends-on ("types" "binary-trees"
                                                    "red-black-trees"))
               (:file "iterator" :depends-on ("types" "binary-trees"))
               (:file "utils" :depends-on ("binary-trees"))
               (:static-file "LICENSE")
               (:static-file "README")
               (:static-file "NEWS")
               (:static-file "TODO")))

(defpackage :trees-tests
  (:use :cl))

(defmethod perform ((op test-op) (c (eql (find-system :trees))))
  (oos 'test-op 'trees-tests))

;;; A tester's job is never done!
(defmethod operation-done-p ((op test-op) (c (eql (find-system :trees))))
  nil)

(asdf:defsystem :trees-tests
  :depends-on (:trees)
  :version "0.3"
  :in-order-to ((test-op (load-op :trees-tests)))
  :components ((:file "rt")
               (:file "validate")
               (:file "tree-test" :depends-on ("rt" "validate"))))

(defmethod operation-done-p ((op test-op)
                             (c (eql (find-system :trees-tests))))
  nil)

(defmethod perform ((op test-op) (c (eql (find-system :trees-tests))))
  (or (funcall (intern "DO-TESTS" (find-package "RTEST")))
      (error "TEST-OP failed for TREES-TESTS")))
