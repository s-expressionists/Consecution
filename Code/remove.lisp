(cl:in-package #:consecution)

(defmethod remove
    (item sequence &key from-end test test-not start end count key)
  (declare (ignore item from-end test test-not start end count key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod remove
    (item (list list) &key from-end test test-not (start 0) end count key)
  (with-test-function (test test test-not)
    (with-key-function (key key)
      (remove-from-list
       (lambda (element) (test item (key element)))
       list from-end start end count))))

(seal-domain #'remove '(t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod remove
      (item (vector #1#) &key from-end test test-not (start 0) end count key)
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (remove-from-vector
         (lambda (element) (test item (key element)))
         vector from-end start end count)))))

(seal-domain #'remove '(t vector))
