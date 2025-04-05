(cl:in-package #:consecution)

(defun power-of-two-p (x)
  (and (integerp x)
       (plusp x)
       (zerop (logand x (1- x)))))

(defun type= (type-1 type-2)
  (multiple-value-bind (1<=2 success)
      (subtypep type-1 type-2)
    (if (not success)
        (values nil (nth-value 1 (subtypep type-2 type-1)))
        (if (not 1<=2)
            (values nil success)
            (subtypep type-2 type-1)))))

(declaim (inline function-designator-function))
(defun function-designator-function (function-designator)
  (typecase function-designator
    (function function-designator)
    (symbol (coerce function-designator 'function))
    (t (error 'must-be-function-designator
              :datum function-designator))))

(declaim (inline canonicalize-count))
(defun canonicalize-count (count)
  (typecase count
    (null +most-positive-vector-length+)
    ((integer * (0)) 0)
    ((integer 0 *) (min count +most-positive-vector-length+))
    (otherwise
     (error 'must-be-count
            :datum count))))

;; Note: Some macros rely on the fact that CLASS-SUBCLASSES sorts its
;; entries most-specific-first.
(defun class-subclasses (class)
  (let ((table (make-hash-table))
        (subclasses '()))
    (labels
        ((push-subclasses (class)
           (unless (gethash class table)
             (setf (gethash class table) t)
             (push class subclasses)
             (mapc #'push-subclasses
                   (closer-mop:class-direct-subclasses class)))))
      (push-subclasses class)
      subclasses)))

(defun vector-class-element-type (sequence-class)
  (let ((prototype (coerce nil sequence-class)))
    (check-type prototype vector)
    (let ((direct-subclasses
            (closer-mop:class-direct-subclasses (class-of prototype))))
      (if (null direct-subclasses)
          (array-element-type prototype)
          `(or ,@(mapcar #'vector-class-element-type direct-subclasses))))))

(defparameter *vector-classes*
  (mapcar #'class-name (class-subclasses (find-class 'vector))))

(defparameter *specialized-vector-classes*
  (loop for vector-class in *vector-classes*
        unless (closer-mop:class-direct-subclasses (find-class vector-class))
          collect vector-class))

;; Some implementations provide a very long list of vector classes, e.g.,
;; (simple-array (unsigned-byte 63) (*)).  Creating a special variant of
;; each sequence function and for each of these vector classes increases
;; compile times and code bloat substantially, so we prune this list a
;; little bit.
(defparameter *relevant-vector-classes*
  (loop for vector-class in *vector-classes*
        for element-type = (vector-class-element-type vector-class)
        when (or (member element-type '(t character base-char fixnum bit))
                 (member element-type '(short-float single-float double-float long-float))
                 (and (consp element-type)
                      (or (eql (car element-type) 'complex)
                          (and (member (car element-type) '(unsigned-byte signed-byte))
                               (power-of-two-p (cadr element-type))))))
          collect vector-class))

(defmacro replicate-for-each (symbol items &body body)
  (check-type symbol symbol)
  `(progn
     ,@(loop for item in items append (subst item symbol body))))

(defmacro replicate-for-each-subclass (symbol class &body body)
  `(replicate-for-each ,symbol ,(mapcar #'class-name (class-subclasses (find-class class))) ,@body))

(defmacro replicate-for-each-vector-class (symbol &body body)
  `(replicate-for-each ,symbol ,*relevant-vector-classes* ,@body))

;;; A vector class is compatible with another vector class, if elements of
;;; the former can be stored in the latter, i.e., when the intersection of
;;; both element types is non-empty.
(defmacro replicate-for-all-compatible-vector-classes (symbol-1 symbol-2 &body body)
  `(progn
     ,@(loop for vector-class in *relevant-vector-classes*
             collect
             `(replicate-for-each ,symbol-1 (,vector-class)
                (replicate-for-each ,symbol-2 ,(compatible-vector-classes vector-class)
                  ,@body)))))

(defun compatible-vector-classes (vector-class)
  (let ((type-1 (vector-class-element-type vector-class)))
    (if (subtypep type-1 'nil)
        '()
        (loop for class in *vector-classes*
              unless
              (let ((type-2 (vector-class-element-type class)))
                (or (subtypep type-2 'nil)
                    (subtypep `(and ,type-1 ,type-2) nil)))
                collect class))))

(declaim (inline shrink-vector))
(defun shrink-vector (vector new-length)
  (declare (vector vector))
  (cond ((= (length vector) new-length)
         vector)
        ((array-has-fill-pointer-p vector)
         (setf (fill-pointer vector) new-length)
         vector)
        (t
         (subseq vector 0 new-length))))

(defvar *vector-prototype-table* (make-hash-table :test #'equal))

(defun vector-prototype (element-type)
  (if (eql element-type '*)
      (load-time-value (vector))
      (let ((uaet (upgraded-array-element-type element-type)))
        (multiple-value-bind (prototype present-p)
            (gethash uaet *vector-prototype-table*)
          (if present-p
              prototype
              (setf (gethash uaet *vector-prototype-table*)
                    (make-array 0 :element-type uaet)))))))

(define-compiler-macro vector-prototype (&whole form element-type)
  (declare (notinline vector-prototype))
  (if (constantp element-type)
      `',(vector-prototype (eval element-type))
      form))

(defun hash-table-test-p (test)
  (or (eql test 'eq)
      (eql test 'eql)
      (eql test 'equal)
      (eql test 'equalp)
      (eql test #'eq)
      (eql test #'eql)
      (eql test #'equal)
      (eql test #'equalp)))

(defmacro with-collectors (collectors &body body)
  "Within the lexical scope of BODY, bind the specified collectors.  Each
entry in COLLECTORS must be a list of two valid function names - RESULT and
GATHER.  Each such pair of function names is bound to local functions, with
the following behavior:

The function named GATHER takes a single argument, stores this argument in
some unspecified way, and returns this argument.

The function named RESULT takes either zero or one object.  It returns the
list of all objects that have been stored by calls to the function named
GATHER.  If supplied, the optional argument is used as the tail of the list
being returned.  Calling the function named RESULT more than once results
in undefined behavior.

Examples:

  (with-collectors ((odds collect-odd)
                    (evens collect-even))
    (loop for n below 8 do
      (if (oddp n)
          (collect-odd n)
          (collect-even n)))
    (values (odds) (evens)))

 => (1 3 5 7)
 => (0 2 4 6)

 (with-collectors ((numbers collect-number))
   (collect-number 2)
   (collect-number 3)
   (numbers '(4 5 6))) ; Here, we explicitly supply a tail.

 => (2 3 4 5 6)
"
  (if (null collectors)
      `(progn ,@body)
      `(with-collector ,(first collectors)
         (with-collectors ,(rest collectors)
           ,@body))))

(defmacro with-collector ((result gather) &body body)
  (let ((head (gensym))
        (tail (gensym)))
    `(let* ((,head (cons nil nil))
            (,tail ,head))
       (declare (cons ,head ,tail) (dynamic-extent ,head))
       (flet ((,result (&optional (tail nil tail-supplied-p))
                (when tail-supplied-p
                  (setf (cdr ,tail) tail))
                (cdr ,head))
              (,gather (object)
                (let ((cons (cons object nil)))
                  (setf (cdr ,tail) cons)
                  (setf ,tail cons)
                  object)))
         (declare (inline ,result ,gather))
         ,@body))))
