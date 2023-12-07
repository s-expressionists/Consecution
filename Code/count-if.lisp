(cl:in-package #:consecution)

(declaim (declaration method-properties))

(defmethod count-if (predicate sequence &key from-end start end key)
  (declare (ignore predicate from-end start end key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod count-if (predicate (list list) &key from-end (start 0) end key)
  (declare (method-properties inlineable))
  (let ((count 0))
    (declare (list-length count))
    (with-predicate (predicate predicate)
      (with-key-function (key key)
        (for-each-relevant-cons (cons index list start end from-end)
          (when (predicate (key (car cons)))
            (incf count)))))
    count))

(seal-domain #'count-if '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod count-if (predicate (vector #1#) &key from-end (start 0) end key)
    (declare (method-properties inlineable))
    (let ((count 0))
      (declare (vector-length count))
      (with-predicate (predicate predicate)
        (with-key-function (key key)
          (for-each-relevant-element (element index vector start end from-end)
            (when (predicate (key element))
              (incf count)))))
      count)))

(seal-domain #'count-if '(t vector))
