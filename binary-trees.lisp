(in-package :trees)

(unless (assoc :normal *binary-tree-info*)
  (push (list :normal #'make-tree-node nil nil) *binary-tree-info*))

(defun emptyp (tree)
  (zerop (size tree)))

(declaim (inline rotate-left rotate-right))
(defun rotate-left (node)
  (let ((c (right node)))
    (setf (right node) (left c)
          (left c) node)
    (incf (rank c) (rank node))
    c))
(defun rotate-right (node)
  (let ((c (left node)))
    (setf (left node) (right c)
          (right c) node)
    (decf (rank node) (rank c))
    c))

(defun update-ranks (direction-stack insertedp)
  (dolist (x direction-stack (values))
    (let ((direction (cdr x)))
      (when (eq direction 'left)
        (if insertedp
            (incf (rank (car x)))
            (decf (rank (car x))))))))

(declaim (inline insert-child-for-stack-entry set-root-or-entry-child))
(defun insert-child-for-stack-entry (entry child)
  (declare (type cons entry))
  (if (eq (cdr entry) 'left)
      (setf (left (car entry)) child)
      (setf (right (car entry)) child)))

(defun set-root-or-entry-child (tree entry child)
  (if (null entry)
      (setf (root tree) child)
      (insert-child-for-stack-entry entry child)))

;;; Locates the place where we should place ITEM in TREE.  Returns two
;;; values: whether we should insert and a stack of nodes visited and
;;; the direction we traveled from each node.  The newly-created node
;;; should be a child of the node of the first entry on the stack.
(defun find-insertion-point (tree item-key)
  (let ((pred (pred tree))
        (test (test tree))
        (key (key tree)))
    (declare (type function pred test key))
    (do ((node (root tree))
         (direction-stack nil))
        ((eq node nil)
         (values nil direction-stack item-key))
      (declare (type (or null tree-node) node))
      (let ((node-key (funcall key (datum node))))
        (cond
          ((funcall test node-key item-key)
           (return-from find-insertion-point (values node direction-stack item-key)))
          ((funcall pred item-key node-key)
           (push (cons node 'left) direction-stack)
           (setf node (left node)))
          (t
           (push (cons node 'right) direction-stack)
           (setf node (right node))))))))

(defun insert (item tree)
  "Attempt to insert ITEM into TREE.  ITEM must be of a suitable type
for TREE's key function, and the key returned from calling said function
must be of a suitable type for TREE's comparison and equality functions.
Returns two values; the first is the key of ITEM and the second
indicates whether ITEM was inserted or not."
  (declare (type binary-tree tree))
  (multiple-value-bind (presentp direction-stack item-key)
      (find-insertion-point tree (funcall (key tree) item))
    (unless presentp
      (update-ranks direction-stack t)
      (incf (size tree))
      (let ((new-node (funcall (nodegen tree) item)))
        (declare (type tree-node new-node))
        (incf (modcount tree))
        (cond
          (direction-stack
           (insert-child-for-stack-entry (first direction-stack) new-node))
          (t
           (setf (root tree) new-node)))
        (let ((rebalancer (rebalance/insert tree)))
          (when rebalancer
            (funcall rebalancer tree direction-stack)))))
    (values item-key (null presentp))))

(declaim (inline lower-bound-node-with-path))
(defun lower-bound-node-with-path (key tree pathp)
  (let ((pred (pred tree))
        (%key (key tree)))
    (declare (type function pred %key))
    (labels ((locate-node (node candidate path)
               (cond
                 ((null node) (values candidate path))
                 ((funcall pred key (funcall %key (datum node)))
                  (locate-node (left node) candidate
                               (when pathp
                                 (cons (cons node 'left) path))))
                 (t
                  (locate-node (right node) node
                               (when pathp
                                 (cons (cons node 'right) path)))))))
      (locate-node (root tree) nil nil))))
(declaim (notinline lower-bound-node-with-path))

(defun lower-bound-node (key tree)
  "Return the node in TREE possessing a key which is equal to or
less than KEY."
  (lower-bound-node-with-path key tree nil))

(defun lower-bound (key tree)
  "Return the item in TREE possessing a key which is equal to or less
than KEY.  Returns NIL if there is no such item."
  (let ((node (lower-bound-node key tree)))
    (and node (datum node))))

(declaim (inline upper-bound-node-with-path))
(defun upper-bound-node-with-path (key tree pathp)
  (let ((pred (pred tree))
        (%key (key tree)))
    (declare (type function pred %key))
    (labels ((locate-node (node candidate path)
               (cond
                 ((null node) (values candidate path))
                 ((funcall pred key (funcall %key (datum node)))
                  (locate-node (left node) node
                               (when pathp (cons (cons node 'left) path))))
                 (t
                  (locate-node (right node) candidate
                               (when pathp (cons (cons node 'right) path)))))))
      (locate-node (the tree-node (root tree)) nil nil))))
(declaim (notinline upper-bound-node-with-path))

(defun upper-bound-node (key tree)
  "Return the node in TREE possessing a key which is equal to or greater
than KEY."
  (upper-bound-node-with-path key tree nil))

(defun upper-bound (key tree)
  "Return the item in TREE possessing a key which is equal to or
greater than KEY.  Returns NIL if there is no such item."
  (let ((node (upper-bound-node key tree)))
    (and node (datum node))))

(defun find-node-with-key (tree key)
  "Find the node in TREE with key KEY.  Might return the null node if no
such node can be found."
  (let ((node (lower-bound-node key tree)))
    (and node
         (funcall (test tree) key (funcall (key tree) (datum node)))
         node)))

(defun find (key tree)
  "Find the item in TREE whose key is KEY and returns the associated item
and T as multiple values, or returns NIL and NIL if no such item exists."
  (let ((node (find-node-with-key tree key)))
    (if node
        (values (datum node) t)
        (values nil nil))))

(defun delete-node (tree node direction-stack)
  (decf (size tree))
  (update-ranks direction-stack nil)
  (let ((parent (caar direction-stack))
        (direction (cdar direction-stack)))
    (flet ((move-node (x)
             (if (null parent)
                 (setf (root tree) x)
                 (if (eq direction 'left)
                     (setf (left parent) x)
                     (setf (right parent) x)))
             x))
      (let ((r (right node)))
        (cond
          ((null r)
           (values (move-node (left node)) direction-stack))
          ((null (left r))
           (setf (left r) (left node)
                 (rank r) (rank node))
           (values (move-node r)
                   (cons (cons r 'right) direction-stack)))
          (t
           ;; find NODE's in-order successor
           (let ((placeholder (cons nil 'right))
                 (parent (first direction-stack)))
             (push placeholder direction-stack)
             (loop
                (push (cons r 'left) direction-stack)
                (let ((succ (left r)))
                  (when (null (left succ))
                    (decf (rank r))
                    ;; move SUCC into NODE's place
                    (setf (left r) (right succ)
                          (left succ) (left node)
                          (right succ) (right node)
                          (rank succ) (rank node))
                    (if (null parent)
                        (setf (root tree) succ)
                        (insert-child-for-stack-entry parent succ))
                    (setf (car placeholder) succ)
                    (return-from delete-node (values (move-node succ) direction-stack)))
                  (decf (rank r))
                  (setf r succ))))))))))

(defun delete (key tree)
  "Attempt to remove the item with KEY from TREE.
Returns the item and T as multiple values on success, NIL and NIL on
failure."
(declare (type binary-tree tree))
  (multiple-value-bind (node direction-stack item-key)
      (find-insertion-point tree key)
    (declare (ignore item-key))
    (if node
        (multiple-value-bind (replacement new-stack)
            (delete-node tree node direction-stack)
          (incf (modcount tree))
          (let ((rebalancer (rebalance/delete tree)))
            (when rebalancer
              (funcall rebalancer tree node replacement new-stack)))
          (values (datum node) t))
        (values nil nil))))

(defun minimum-node (root)
  (do ((node root (left node))
       (parent nil node))
      ((eq node nil) parent)))

(defun minimum (tree)
  "Return the item with the minimum key in TREE.  It is an error to ask
for the minimum item of an empty tree."
  (if (zerop (size tree))
      (error "Empty tree")
      (datum (minimum-node (root tree)))))

(defun maximum-node (root)
  (do ((node root (right node))
       (parent nil node))
      ((eq node nil) parent)))

(defun maximum (tree)
  "Return the item with the maximum key in TREE.  It is an error to ask
for the maximum item of an empty tree."
  (if (zerop (size tree))
      (error "Empty tree")
      (datum (maximum-node (root tree)))))

(defun select-node-with-path (tree k pathp)
  (labels ((select-loop (node k path)
             (let ((rank (1- (rank node))))
               (cond
                 ((= k rank) (values node path))
                 ((< k rank) (select-loop (left node) k
                                          (when pathp
                                            (cons (cons node 'left) path))))
                 (t (select-loop (right node) (- k rank 1)
                                 (when pathp
                                   (cons (cons node 'right) path))))))))
    (cond
      ((or (minusp k)
           (>= k (size tree))) (error "Invalid index value"))
      (t (select-loop (root tree) k nil)))))

(defun select-node (tree k)
  (select-node-with-path tree k nil))

(defun select (tree k)
  "Return the Kth item (zero-based) in TREE."
  (datum (select-node tree k)))
