(in-package :trees)

(defun update-balance-factors (tree direction-stack)
  (loop with y = (root tree)
     with parent = nil
     with tail = nil
     with reversed-stack = (nreverse direction-stack)
     for x on reversed-stack
     and xp = nil then (car x)
     do (let ((node (caar x)))
          (when (not (zerop (balance-info node)))
            (setf y node parent xp tail x)))
     finally
       (return
         ;; If TAIL is NIL, then we have an entire path of nodes with
         ;; zero balance factors and the whole path needs to be
         ;; adjusted.
         (dolist (p (if (null tail)
                        reversed-stack
                        tail)
                  (values y parent))
           (let ((node (car p)))
             (if (eq (cdr p) 'left)
                 (decf (balance-info node))
                 (incf (balance-info node))))))))

(defun avl-rebalance/insert (tree direction-stack)
  (when direction-stack
    (multiple-value-bind (y parent-entry)
        (update-balance-factors tree direction-stack)
      (case (balance-info y)
        (#.+avl-falls-left+
         (let ((x (left y)) w)
           (ecase (balance-info x)
             (#.+avl-leans-left+
              (setf w (rotate-right y)
                    (balance-info x) +avl-equal+
                    (balance-info y) +avl-equal+))
             (#.+avl-leans-right+
              (setf (left y) (rotate-left x)
                    w (rotate-right y))
              (case (balance-info w)
                (#.+avl-leans-left+
                 (setf (balance-info x) 0
                       (balance-info y) +avl-leans-right+))
                (#.+avl-equal+
                 (setf (balance-info x) 0
                       (balance-info y) 0))
                (#.+avl-leans-right+
                 (setf (balance-info x) +avl-leans-left+
                       (balance-info y) 0)))
              (setf (balance-info w) +avl-equal+)))
           (set-root-or-entry-child tree parent-entry w)))
        (#.+avl-falls-right+
         (let ((x (right y)) w)
           (ecase (balance-info x)
             (#.+avl-leans-right+
              (setf w (rotate-left y)
                    (balance-info x) +avl-equal+
                    (balance-info y) +avl-equal+))
             (#.+avl-leans-left+
              (setf (right y) (rotate-right x)
                    w (rotate-left y))
              (case (balance-info w)
                (#.+avl-leans-right+
                 (setf (balance-info x) 0
                       (balance-info y) +avl-leans-left+))
                (#.+avl-equal+
                 (setf (balance-info x) 0
                       (balance-info y) 0))
                (#.+avl-leans-left+
                 (setf (balance-info x) +avl-leans-right+
                       (balance-info y) 0)))
              (setf (balance-info w) +avl-equal+)))
           (set-root-or-entry-child tree parent-entry w)))))))

(defun avl-rebalance/delete (tree node replacement stack)
  (loop
     initially (unless (and (null (right node))
                            (eq replacement (left node)))
                 (setf (balance-info replacement) (balance-info node)))
     for (top . rest) on stack
     do (let ((y (car top))
              (direction (cdr top)))
          (macrolet ((frob (dir opp r1 r2)
                       (flet ((ifleft (dir f1 f2)
                                (if (eq dir 'left)
                                    f1
                                    f2))
                              (leftinfo (dir a)
                                (if (eq dir 'left) a (- a))))
                         `(progn
                            (,(ifleft dir 'incf 'decf) (balance-info y))
                            (case (balance-info y)
                              (,(leftinfo dir +avl-leans-right+)
                               (loop-finish))
                              (,(leftinfo dir +avl-falls-right+)
                               (let ((x (,opp y)))
                                 (case (balance-info x)
                                   (,(leftinfo dir +avl-leans-left+)
                                    (let ((w (,dir x)))
                                      (setf (,opp y) (,r1 x))
                                      (let ((r (,r2 y)))
                                        (assert (eq w r))
                                        (case (balance-info w)
                                          (,(leftinfo dir +avl-leans-right+)
                                           (setf (balance-info x) +avl-equal+
                                                 (balance-info y) ,(leftinfo dir +avl-leans-left+)))
                                          (,(leftinfo dir +avl-equal+)
                                           (setf (balance-info x) +avl-equal+
                                                 (balance-info y) +avl-equal+))
                                          (,(leftinfo dir +avl-leans-left+)
                                           (setf (balance-info x) ,(leftinfo dir +avl-leans-right+)
                                                 (balance-info y) +avl-equal+)))
                                        (setf (balance-info w) 0)
                                        (set-root-or-entry-child tree (first rest) w))))
                                   (t
                                    (let ((r (,r2 y)))
                                      (set-root-or-entry-child tree (first rest) r)
                                      (cond
                                        ((= (balance-info x) +avl-equal+)
                                         (setf (balance-info x) ,(leftinfo dir +avl-leans-left+)
                                               (balance-info y) ,(leftinfo dir +avl-leans-right+))
                                         (loop-finish))
                                        (t
                                         (setf (balance-info x) +avl-equal+
                                               (balance-info y) +avl-equal+)))))))))))))
            (case direction
              (left (frob left right rotate-right rotate-left))
              (right (frob right left rotate-left rotate-right)))))))

(unless (assoc :avl *binary-tree-info*)
  (push (list :avl
              #'make-avl-node
              #'avl-rebalance/insert
              #'avl-rebalance/delete)
        *binary-tree-info*))
