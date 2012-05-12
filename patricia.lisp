(in-package :trees)

(defmethod make-binary-tree-node ((tree patricia-tree) item)
  (let ((null-node (sentinel-node tree)))
    (make-instance 'patricia-tree-node
                   :left null-node
                   :right null-node
                   :parent null-node
                   :datum item)))

(defmethod make-binary-tree ((type (eql :patricia)) &key compfun eqfun keyfun)
  (let ((sentinel-node (make-sentinel-node 'patricia-tree-node)))
    (make-instance 'patricia-tree
                   :compfun compfun
                   :eqfun eqfun
                   :keyfun keyfun)))

(defmethod insert-at-node ((tree patricia-tree) item parent direction-stack)
  )

(defmethod tree-delete-nonempty ((tree patricia-tree) deleted child low-subtree)
  )
