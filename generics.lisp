(in-package :trees)

(defvar *binary-tree-info* nil)

(defun pred-translate (type pred)
  (case type
    (:interval
     (lambda (a b)
       (typecase a
         (interval (funcall pred
                            (interval-start a)
                            (interval-start b)))
         (t (funcall pred a b)))))
    (t pred)))

(defun make-binary-tree (type pred &key key test)
  "Create a binary tree based on TYPE.  Current acceptable values for TYPE are:

  :NORMAL - a normal binary tree, with no rebalancing
  :RED-BLACK - a red-black tree
  :AVL - an AVL tree
  :AA - an AA tree.
  :INTERVAL - an augmented red-black tree for storing intervals

PRED specifies the ordering relation.  KEY specifies how to access the
data for comparison.  TEST is optional and, if given, specifies how to
compare two keys for equality."
  (let* ((pred (pred-translate type (coerce pred 'function)))
         (key (coerce key 'function))
         (test (if test
                   (coerce test 'function)
                   (lambda (x y)
                     (not (or (funcall pred x y)
                              (funcall pred y x))))))
         (specifics (assoc type *binary-tree-info*)))
    (unless specifics
      (error "Unknown tree kind ~A" type))
    (apply #'%make-binary-tree pred key test (cdr specifics))))
