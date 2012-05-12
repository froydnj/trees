(in-package :trees)

(defmethod print-object ((node tree-node) stream)
  (print-unreadable-object (node stream)
    (format stream "btn ~A, rank ~A"
            (datum node)
            (rank node))))

(defmethod print-object ((node avl-tree-node) stream)
  (print-unreadable-object (node stream)
    (format stream "avltn ~A/~A, rank ~A"
            (balance-info node)
            (datum node)
            (rank node))))

(defmethod print-object ((node red-black-tree-node) stream)
  (print-unreadable-object (node stream)
    (format stream "rbtn ~A/~A, rank ~A"
            (color node)
            (datum node)
            (rank node))))

(defmethod print-object ((node aa-tree-node) stream)
  (print-unreadable-object (node stream)
    (format stream "aatn ~A/~A, rank ~A"
            (level node)
            (datum node)
            (rank node))))

(defun indent-to-level (n &optional (stream *standard-output*))
  (dotimes (i n)
    (write-char #\Space stream)))

(defun pprint-tree (tree &optional (stream *standard-output*))
  (labels ((recursive-print (node level char)
             (indent-to-level level stream)
             (write-char char stream)
             (write-char #\Space stream)
             (prin1 node stream)
             (terpri stream)
             (unless (null (left node))
               (recursive-print (left node) (1+ level) #\l))
             (unless (null (right node))
               (recursive-print (right node) (1+ level) #\r))))
    (if (null (root tree))
        (format stream "empty tree~%")
        (recursive-print (root tree) 0 #\R))
    (values)))
