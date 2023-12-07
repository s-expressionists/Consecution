(cl:in-package #:consecution)

(defmethod remove-if (test sequence &key from-end start end count key)
  (declare (ignore test from-end start end count key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod remove-if (test (list list) &key from-end (start 0) end count key)
  (with-predicate (predicate test)
    (with-key-function (key key)
      (remove-from-list
       (lambda (element) (predicate (key element)))
       list from-end start end count))))

(seal-domain #'remove-if '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod remove-if (test (vector #1#) &key from-end (start 0) end count key)
    (with-predicate (predicate test)
      (with-key-function (key key)
        (remove-from-vector
         (lambda (element) (predicate (key element)))
         vector from-end start end count)))))

(seal-domain #'remove-if '(t vector))
