(cl:in-package #:consecution)

(declaim (declaration method-properties))

(defmethod position-if-not (predicate sequence &key from-end start end key)
  (declare (ignore predicate from-end start end key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod position-if-not (predicate (list list) &key from-end (start 0) end key)
  (declare (method-properties inlineable))
  (with-predicate (predicate predicate)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (unless (predicate (key element))
            (return-from position-if-not index)))))))

(seal-domain #'position-if-not '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod position-if-not (predicate (vector #1#) &key from-end (start 0) end key)
    (declare (method-properties inlineable))
    (with-predicate (predicate predicate)
      (with-key-function (key key)
        (for-each-relevant-element (element index vector start end from-end)
          (unless (predicate (key element))
            (return-from position-if-not index)))))))

(seal-domain #'position-if-not '(t vector))
