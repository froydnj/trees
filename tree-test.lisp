(in-package :trees-tests)

(defconstant +max-upper-bound+ (expt 2 15))

(defvar *execution-trace*)
(defvar *saved-trace*)
(defvar *print-while-building* nil)
(defvar *validate-while-building* nil)
(defvar *building-uses-delete* nil)
(defvar *let-pred-determine-equivalency* nil)

(defun make-integer-tree (tree-kind)
  ;; Use explicit lambdes to make SBCL emit calls to GENERIC assembly
  ;; procedures, rather than the full functions; the GENERIC versions
  ;; will be faster, as they contain early outs for fixnums.
  (trees:make-binary-tree tree-kind #'(lambda (x y)
                                         (< x y))
                           :key #'identity
                           :test (unless *let-pred-determine-equivalency*
                                   #'(lambda (x y) (= x y)))))

(defun make-bitset (upper-bound)
  (make-array upper-bound :element-type 'bit :initial-element 0))

(defun make-trace-buffer (&optional (size 400))
  (make-array size :element-type '(unsigned-byte 16)
              :fill-pointer 0
              :adjustable t))

(defun add-trace-action (insertp element)
  (vector-push-extend (dpb insertp (byte 1 15) element) *execution-trace*))

(defun insert-action-p (bits)
  (not (logbitp 15 bits)))

(defun action-element (bits)
  (ldb (byte 15 0) bits))

(defun test-with-randomly-built-tree (kind test &optional (size 200))
  (multiple-value-bind (tree bitvector count)
      (build-random-tree kind size :delete t)
    (declare (ignore count))
    (funcall test tree bitvector)))

(defun check-equivalency (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop
     for start = 0 then (1+ x)
     for x = (position 1 bitvector :start start)
     while x
     do (assert (trees:find x tree))
     finally (return t)))

(defun check-selection (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (unless (zerop (trees:size tree))
    (loop for start = 0 then (1+ x)
       for x = (position 1 bitvector :start start)
       for i from 0
       while x
       do (assert (= x (trees:select tree i)))
       finally (return t))))

(defun check-forward-position-t (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop for start = 0 then (1+ x)
     for x = (position 1 bitvector :start start)
     for i from 0
     while x
     do (assert (= i (trees:position x tree)))
     finally (return t)))

(defun check-forward-position-nil (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop for start = 0 then (1+ x)
     for x = (position 0 bitvector :start start)
     while x
     do (assert (eq (trees:position x tree) nil))
     finally (return t)))

(defun check-backward-position-t (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop for end = nil then x
     for x = (position 1 bitvector :from-end t :end end)
     for i downfrom (1- (trees:size tree))
     while x
     do (assert (= i (trees:position x tree)))
     finally (return t)))

(defun check-backward-position-nil (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop for end = nil then x
     for x = (position 0 bitvector :from-end t :end end)
     while x
     do (assert (eq (trees:position x tree :from-end t) nil))
     finally (return t)))

(defun check-forward-iteration (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop with iter = (trees::make-iterator tree :forwardp t)
     for start = 0 then (1+ x)
     for x = (position 1 bitvector :start start)
     for z = (funcall iter)
     while x
     do (assert (= (trees::datum z) x))
     finally (return t)))

(defun check-backward-iteration (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop with iter = (trees::make-iterator tree :forwardp nil)
     for end = nil then y
     for y = (position 1 bitvector :from-end t :end end)
     for w = (funcall iter)
     while y
     do (assert (= y (trees::datum w)))
     finally (return t)))

(defun check-dotree (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (let ((new (bit-xor bitvector bitvector)))
    (trees:dotree (x tree)
      (setf (aref new x) 1))
    (assert (not (mismatch new bitvector)))
    t))

(defun check-reduce (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop with tree-sum = (trees:reduce tree #'+)
     for start = 0 then (1+ x)
     for x = (position 1 bitvector :start start)
     while x
     sum x into sum
     finally
       (assert (= tree-sum sum))
       (return t)))

(defun check-reduce-from-end (tree bitvector)
  (declare (type simple-bit-vector bitvector))
  (loop with tree-diff = (trees:reduce tree #'- :initial-value 0 :from-end t)
     for start = 0 then (1+ x)
     for x = (position 1 bitvector :start start)
     while x
     collect x into list
     finally
       (assert (= tree-diff (reduce #'- list :initial-value 0 :from-end t)))
       (return t)))

(defparameter *tree-checkers*
  (list #'check-equivalency
        #'check-selection
        #'check-forward-position-t
        #'check-forward-position-nil
        #'check-backward-position-t
        #'check-backward-position-nil
        #'check-forward-iteration
        #'check-backward-iteration
        #'check-dotree
        #'check-reduce
        #'check-reduce-from-end))

(defun build-random-tree (tree-type upper-bound
                          &key (delete *building-uses-delete*)
                          trace)
  (when (> upper-bound +max-upper-bound+)
    (error "upper bound ~A is too large" upper-bound))
  (let ((*execution-trace* (when trace (make-trace-buffer))))
    (loop with tree = (make-integer-tree tree-type)
       with bitvector of-type simple-bit-vector = (make-bitset upper-bound)
       with size = 0
       with insert-count = 0
       with delete-count = 0
       with insert-limit = (truncate upper-bound 2)
       with delete-limit = (if delete (truncate upper-bound 2) 0)
       with validatep = *validate-while-building*
       until (and (>= insert-count insert-limit)
                  (>= delete-count delete-limit))
       do (let ((element (random upper-bound))
                (which (if delete (random 2) 0)))
            (case which
              (0                        ; insert
               (multiple-value-bind (key insertedp) (trees:insert element tree)
                 (cond
                   (insertedp
                    (when trace (add-trace-action which element))
                    (assert (= 0 (aref bitvector element)))
                    (setf (aref bitvector element) 1)
                    (incf size)
                    (incf insert-count))
                   (t
                    (assert (= 1 (aref bitvector element)))))
                 (assert (= key element))
                 (assert (trees:find element tree))))
              (1                        ; delete
               (multiple-value-bind (data successp) (trees:delete element tree)
                 (cond
                   (successp
                    (when trace (add-trace-action which element))
                    (assert (= element data))
                    (assert (= (aref bitvector element) 1))
                    (setf (aref bitvector element) 0)
                    (decf size)
                    (incf delete-count))
                   (t
                    (assert (= (aref bitvector element) 0))))
                 (assert (not (trees:find element tree))))))
            (when validatep
              (assert (validate-tree tree)))
            (assert (= size (trees:size tree))))
       finally (check-equivalency tree bitvector)
         (validate-tree tree)
         (return (values tree bitvector (+ insert-count delete-count))))))

(defun build-tree-from-trace (tree-type trace maker insert delete printer)
  (loop with tree = (funcall maker tree-type)
     for action across trace
     for i from 0
     do (funcall (if (insert-action-p action) insert delete)
                 (action-element action) tree)
       (when *print-while-building*
         (format *trace-output* "step ~D: ~A ~D~%" i
                 (if (insert-action-p action) "insert" "delete")
                 (action-element action))
         (funcall printer tree *trace-output*))
     finally (return tree)))

(defun build-new-tree-from-trace (tree-type trace)
  (build-tree-from-trace tree-type trace
                         #'make-integer-tree
                         #'trees:insert
                         #'trees:delete
                         #'trees::pprint-tree))

(defun build-and-compare (tree-type deletionp)
  (handler-bind ((error #'(lambda (c)
                            (assert *execution-trace*)
                            (setf *saved-trace* *execution-trace*)
                            (let ((*print-while-building* t))
                              (build-new-tree-from-trace tree-type *execution-trace*))
                            (continue c))))
    (build-random-tree tree-type 200 :delete deletionp :trace t)))

(defun build-and-run-checkers (tree-kind)
  (flet ((build-and-run (x)
           (let ((*let-pred-determine-equivalency* x))
             (multiple-value-bind (tree bitvector count)
                 (build-random-tree tree-kind 200 :delete t)
               (declare (ignore count))
               (dolist (test *tree-checkers* t)
                 (funcall (the function test) tree bitvector))))))
    (handler-case
        (loop repeat 1000 always (and (build-and-run t) (build-and-run nil)))
      (error (c) c)
      (:no-error (value) value))))

(rtest:deftest :normal (build-and-run-checkers :normal) t)

(rtest:deftest :avl (build-and-run-checkers :avl) t)

(rtest:deftest :red-black (build-and-run-checkers :red-black) t)

(rtest:deftest :aa (build-and-run-checkers :aa) t)
