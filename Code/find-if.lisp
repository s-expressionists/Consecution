(cl:in-package #:consecution)

(declaim (declaration method-properties))

(defmethod find-if (predicate sequence &key from-end start end key)
  (declare (ignore predicate from-end start end key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod find-if (predicate (list list) &key from-end (start 0) end key)
  (declare (method-properties inlineable))
  (with-predicate (predicate predicate)
    (with-key-function (key key)
      (for-each-relevant-cons (cons index list start end from-end)
        (let ((element (car cons)))
          (when (predicate (key element))
            (return-from find-if element)))))))

(seal-domain #'find-if '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod find-if (predicate (vector #1#) &key from-end (start 0) end key)
    (declare (method-properties inlineable))
    (with-predicate (predicate predicate)
      (with-key-function (key key)
        (for-each-relevant-element (element index vector start end from-end)
          (when (predicate (key element))
            (return-from find-if element)))))))

(seal-domain #'find-if '(t vector))
