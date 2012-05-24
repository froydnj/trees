(defpackage "BINARY-TREES"
  (:use "COMMON-LISP")
  (:nicknames #:trees)
  (:shadow reduce find delete position)
  (:export #:emptyp
           #:make-binary-tree

           #:binary-tree
           #:avl-tree
           #:red-black-tree
           #:aa-tree
           #:interval-tree

           #:insert
           #:find
           #:delete
           #:size
           #:minimum
           #:maximum
           #:select
           #:position
           #:reduce

           #:upper-bound
           #:lower-bound

           #:interval
           #:interval-start
           #:interval-end
           #:interval-equal
           #:make-interval
           #:find-in-interval

           #:dotree
           ;#:do-tree-range
           ;#:with-tree-iterator

           #:pprint-tree))
