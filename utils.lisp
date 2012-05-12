(in-package :trees)

(defun for-each (func tree forwardp)
  (let ((iter (make-iterator tree :forwardp forwardp)))
    (declare (type function iter))
    (loop (multiple-value-bind (node morep) (funcall iter)
            (unless morep (return-from for-each (values)))
            (funcall func (datum node))))))

(defun tree-for-each (func tree)
  (for-each func tree t))

(defun reverse-tree-for-each (func tree)
  (for-each func tree nil))

;;; We implement this directly in terms of iterators, rather than
;;; re-using TREE-FOR-EACH, so that we can provide for GO tags in the
;;; body of the loop, similarly to DO/DOTIMES/DOLIST.
(defmacro dotree ((obj-var tree-var &optional return-value) &body body)
  (let ((node (gensym))
        (iter (gensym))
        (tree (gensym)))
  `(let* ((,tree ,tree-var)
          (,iter (make-iterator ,tree :forwardp t)))
     (declare (type function ,iter))
     (do ((,node (funcall ,iter) (funcall ,iter)))
         ((null ,node) ,return-value)
       (let ((,obj-var (datum ,node)))
         (tagbody
            ,@body))))))

#||
(defmacro do-tree-range ((obj-var tree-var
                                  &key (type :key)
                                  (lower nil)
                                  (upper nil)) &body body)
  (macrolet ((invalid-type (type)
               `(error "Invalid :type supplied to DO-TREE-RANGE: ~A" ,type)))
    (let* ((node (gensym))
           (tree (gensym))
           (current (gensym))
           (stack (gensym))
           (iterator (gensym))
           (morep (gensym))
           (name (gensym))
           (last (gensym))
           (lower-exp (if lower
                          (cond
                            ((eq type :key)
                             `(upper-bound-node-with-path ,lower ,tree t))
                            ((eq type :index)
                             `(select-node-with-path ,tree ,lower))
                            (t (invalid-type type)))
                          (cond
                            ((or (eq type :key) (eq type :index))
                             `(minimum-node ,tree-var (root ,tree)))
                            (t (invalid-type type)))))
           (upper-exp (if upper
                          (cond
                            ((eq type :key)
                             `(upper-bound-node ,upper ,tree))
                            ((eq type :index)
                             `(select-node ,tree ,upper))
                            (t (invalid-type type)))
                          (cond
                            ((or (eq type :key) (eq type :index)) nil)
                            (t (invalid-type type))))))
      `(let ((,tree ,tree-var))
         (multiple-value-bind (,current ,stack) ,lower-exp
           (loop named ,name
              with ,iterator = (make-iterator ,tree :forwardp t
                                              :current ,current
                                              :stack ,stack)
              with ,last = ,upper-exp
              do (multiple-value-bind (,node ,morep) (funcall ,iterator)
                   (when (or (not ,morep)
                             (eq ,node ,last))
                     (return-from ,name))
                   (let ((,obj-var (datum ,node)))
                     (tagbody
                        ,@body)))))))))

;;; FIXME: FROM-END isn't necessarily very intuitive here.  find out
;;; how regular CL sequence functions treat it (especially with indices)
;;; and rewrite the macro to match.
(defmacro with-tree-iterator ((iter tree &key
                                    (from-end nil) (type :key) (start nil))
                              &body body)
  "Like WITH-HASH-TABLE-ITERATOR; ITER is a name defined via MACROLET
and TREE is a form evaluated to produce a tree.  Successive calls to ITER
return the items in the tree, one by one.

 (ITER) two values.  The first is a boolean that is true if an object from
the tree is returned; the second is an object stored in the tree.

TYPE can be either :KEY or :INDEX and defines how to interpret START.  If
TYPE is :KEY and START is specified, then START is taken to be some key of
the tree from which iteration should begin.  If no such key exists, then
the next greatest key is chosen as the starting point.  If TYPE is :INDEX,
and START is specified, then START is taken to be an index passed to
SELECT-NODE to determine from what object iteration should begin.

If START is not specified, iteration begins from the minimum node of TREE.

FROM-END is currently broken and should not be used."
  (let ((treesym (gensym))
        (n-iter (gensym)))
    `(let ((,n-iter
            (let* ((,treesym ,tree)
                   (node ,(cond
                           ((eq type :key)
                            (if start
                                `(lower-bound-node ,start ,treesym)
                                `(minimum-node ,treesym (root-node ,treesym))))
                           ((eq type :index)
                            (if start
                                `(select-node ,start ,treesym)
                                `(minimum-node ,treesym (root-node ,treesym)))))))
              (labels ((,iter ()
                         (multiple-value-prog1
                             (values (not (null-node-p node ,treesym))
                                     (datum node))
                           (setf node (tree-successor node)))))
                #',iter))))
       (macrolet ((,iter () '(funcall ,n-iter)))
         ,@body))))
||#

(defun reduce (tree function
                    &key key
                    (initial-value nil valuep)
                    (from-end nil))
  (let ((accum (if valuep
                   initial-value
                   (funcall function))))
    (flet ((left-reducer (object)
             (setf accum (funcall function accum (if key
                                                     (funcall key object)
                                                     object))))
           (right-reducer (object)
             (setf accum (funcall function (if key
                                               (funcall key object)
                                               object) accum))))
      (declare (dynamic-extent #'left-reducer #'right-reducer))
      (if from-end
          (reverse-tree-for-each #'right-reducer tree)
          (tree-for-each #'left-reducer tree))
      accum)))

(defun position (key tree &key from-end)
  (multiple-value-bind (node stack item-key) (find-insertion-point tree key)
    (declare (ignore item-key))
    (if (null node)
        node
        (loop with position = (1- (rank node))
           for entry in stack
           unless (eq (cdr entry) 'left)
           do (incf position (rank (car entry)))
           finally (return
                     (if from-end
                         (- (size tree) position)
                         position))))))
