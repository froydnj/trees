(in-package :trees)

(defun extreme-node-with-path (root direction &optional path)
  (do ((node root (funcall direction node))
       (parent nil node))
      ((null node) (values parent path))
    (push node path)))

(defun make-iterator (tree &key
                       forwardp
                       (current nil currentp)
                       (stack nil stackp))
  (declare (type binary-tree tree))
  (let ((modcount (modcount tree)))
    (multiple-value-bind (extremum examine)
                  (if forwardp
                      (values #'left #'right)
                      (values #'right #'left))
      (multiple-value-bind (current stack)
          (if (and currentp stackp)
              (values current stack)
              (extreme-node-with-path (root tree) extremum))
        #'(lambda ()
            (cond
              ((/= modcount (modcount tree))
               (error "~A modified during iteration" tree))
              ((null current)
               (values nil nil))
              (t
               (let* ((next current)
                      (top (pop stack))
                      (node (funcall examine top)))
                 (cond
                   ((null node)
                    (setf current (first stack)))
                   (t
                    (setf (values current stack)
                          (extreme-node-with-path node extremum stack))))
                 (values next t)))))))))
