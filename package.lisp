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

           #:dotree
           ;#:do-tree-range
           ;#:with-tree-iterator

           #:pprint-tree))
