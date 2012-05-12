(in-package :trees)


;;; tree nodes

;;; Structures give better performance than CLOS.

(defstruct (tree-node
             (:conc-name)
             (:constructor make-tree-node (datum))
             (:copier nil))
  (left nil :type (or null tree-node))
  (right nil :type (or null tree-node))
  (rank 1 :type fixnum)
  (datum nil))

(defstruct (avl-tree-node
             (:conc-name)
             (:constructor make-avl-node (datum))
             (:copier nil)
             (:include tree-node))
  (balance-info 0 :type (integer -2 2)))

(defconstant +avl-equal+ 0)
(defconstant +avl-leans-left+ -1)
(defconstant +avl-leans-right+ +1)
(defconstant +avl-falls-left+ -2)
(defconstant +avl-falls-right+ +2)

(deftype red-black-color ()
  "Keywords denoting red-black tree colors."
  '(member :red :black))

(defstruct (red-black-tree-node
             (:conc-name)
             (:constructor make-red-black-node (datum))
             (:copier nil)
             (:include tree-node))
  (color :red :type red-black-color))

(defstruct (aa-tree-node
             (:conc-name)
             (:constructor make-aa-node (datum))
             (:copier nil)
             (:include tree-node))
  (level 1 :type fixnum))

;;; trees

(defstruct (binary-tree
             (:conc-name)
             (:constructor %make-binary-tree (pred key test
                                                   nodegen
                                                   rebalance/insert
                                                   rebalance/delete))
             (:copier nil))
  (test #1=(error "missing arg") :type function :read-only t)
  (key #1# :type function :read-only t)
  (pred #1# :type function :read-only t)
  (size 0 :type fixnum)
  (root nil :type (or null tree-node))
  (modcount 0 :type fixnum)
  (nodegen #1# :type function :read-only t)
  (rebalance/insert nil :type (or null function) :read-only t)
  (rebalance/delete nil :type (or null function) :read-only t))
