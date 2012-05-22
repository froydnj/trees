(in-package :trees)

(defun make-interval-tree-node (datum)
  (let ((interval-node (%make-interval-tree-node datum)))
    (setf (end-val interval-node) (interval-end datum))
    interval-node))

(defun interval-equal (a b)
  (and (equal (interval-start a) (interval-start b))
       (equal (interval-end a) (interval-end b))))

(declaim (inline node-start node-end node-interval))
                 
(defun node-start (node)
  (when node (interval-start (datum node))))

(defun node-end (node)
  (when node (interval-end (datum node))))

(defun node-interval (node)
  (when node (datum node)))

(defun maximize-endval (pred node)
  (flet ((pred-max (a b)
           (if (eq a b) a
               (if (funcall pred a b) b a))))
    (let ((val (node-end node))
          (left (left node))
          (right (right node)))
      (let ((left-max (and left (end-val left)))
            (right-max (and right (end-val right))))
        (setf (end-val node) (cl:reduce
                              #'pred-max (list (or left-max val)
                                               (or right-max val))
                              :initial-value val))))))

(defun interval-rebalance/insert (tree direction-stack)
  (red-black-rebalance/insert tree direction-stack)
  (let ((root (root tree))
        (pred (pred tree)))
    (when direction-stack
        (loop for (node . direction) in direction-stack
              do (when (left node)
                   (maximize-endval pred (left node)))
                 (when (right node)
                   (maximize-endval pred (right node)))
                 (maximize-endval pred node)))
    (maximize-endval pred root)))

(defun interval-rebalance/delete (tree node replacement new-stack)
  (red-black-rebalance/delete tree node replacement new-stack)
  (let ((pred (pred tree)))
    (loop for (node . direction) in new-stack
          do (maximize-endval pred node))))

(defmethod print-object ((object interval-tree-node) stream)
  (format stream "#<itn ~A-~A (max ~A) (~A)>"
          (interval-start (datum object))
          (interval-end (datum object))
          (end-val object)
          (color object)))

(defun node-in-interval-p (pred node interval)
  (when node
    (and (funcall pred (node-start node) (interval-end interval))
         (funcall pred (interval-start interval) (node-end node)))))

(defun find-nodes-in-interval (pred node interval)
  (when node
    (concatenate 'list
                 ;; Left subtree if we're anywhere PRED of end-val
                 (when (funcall pred (interval-start interval) (end-val node))
                   (find-nodes-in-interval pred (left node) interval))
                 (when (node-in-interval-p pred node interval) (list node))
                 ;; Right subtree if we're anywhere (NOT PRED) of node-start
                 (when (not (funcall pred (node-start node) (interval-start interval)))
                   (find-nodes-in-interval pred (right node) interval)))))

(defun find-in-interval (interval tree)
  (let ((interval (typecase interval
                    (interval interval)
                    (cons (make-interval :start (car interval)
                                         :end (cdr interval)))
                    (t (make-interval :start interval
                                      :end interval))))
        (pred (pred tree)))
    (mapcar #'datum (find-nodes-in-interval pred (root tree) interval))))

(if (assoc :interval *binary-tree-info*)
    (setf (cdr (assoc :interval *binary-tree-info*))
          (list #'make-interval-tree-node
                #'interval-rebalance/insert
                #'interval-rebalance/delete))
    (push (list :interval
                #'make-interval-tree-node
                #'interval-rebalance/insert
                #'interval-rebalance/delete)
          *binary-tree-info*))

(defun dotify-interval-node (name node stream)
  (let ((left-name (gensym))
        (right-name (gensym)))
    (format stream "~&   ~A [label=\"(~A-~A) : ~A\",color=~A]"
            name (node-start node) (node-end node)
            (end-val node) (color node))
    (when (left node)
      (format stream "~&   ~A -> ~A" name left-name)
      (dotify-interval-node left-name (left node) stream))
    (when (right node)
      (format stream "~&   ~A -> ~A" name right-name)
      (dotify-interval-node right-name (right node) stream))))

(defun dotify-interval-tree (tree stream)
  (format stream "~&digraph interval {")
  (dotify-interval-node (gensym) (root tree) stream)
  (format stream "~&}~%"))
