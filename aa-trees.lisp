(in-package :trees)

(declaim (inline level*))
(defun level* (x) (if x (level x) 0))

(defun skew (node)
  (let ((x (left node)))
    (when (= (level* x) (level node))
      (setf node (rotate-right node)))
    node))

(defun split (node)
  (let ((x (right node)))
    (when (= (if x
                 (level* (right x))
                 0)
             (level node))
      (setf node (rotate-left node))
      (incf (level node)))
    node))

(defun aa-rebalance/insert (tree direction-stack)
  (when direction-stack
    (loop with new-child = (split (skew (caar direction-stack)))
       for x in (cdr direction-stack)
       for node = (car x)
       do (insert-child-for-stack-entry x new-child)
       (setf new-child (split (skew node)))
       finally (setf (root tree) new-child))))

(defun aa-rebalance/delete (tree node replacement stack)
  ;; This is what I get for trying to do things without sentinels.
  (loop initially (when replacement
                    (setf (level replacement) (level node)))
     for (x . rest) on stack
     do (let* ((node (car x))
               (y (left node))
               (z (right node)))
          (when (let ((level (1- (level node))))
                  (or (< (level* y) level)
                      (< (level* z) level)))
            (decf (level node))
            (when (> (level* z) (level node))
              (setf (level z) (level node)))
            (let ((n (skew node)))
              (set-root-or-entry-child tree (car rest) n)
              (when (right n)
                (setf (right n) (skew (right n))))
              (let ((m (right n)))
                (when (and m (right m))
                  (setf (right m) (skew (right m)))))
              (setf n (split n))
              (set-root-or-entry-child tree (car rest) n)
              (when (right n)
                (setf (right n) (split (right n)))))))))

(unless (assoc :aa *binary-tree-info*)
  (push (list :aa
              #'make-aa-node
              #'aa-rebalance/insert
              #'aa-rebalance/delete)
        *binary-tree-info*))
