(cl:in-package #:consecution)

(defmethod nsubstitute-if (newitem predicate sequence
                           &key from-end start end count key)
  (declare (ignore newitem predicate from-end start end count key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod nsubstitute-if (newitem predicate (list list)
                           &key from-end (start 0) end count key)
  (let ((count (canonicalize-count count))
        (n-substituted 0))
    (with-predicate (predicate predicate)
      (with-key-function (key key)
        (unless (zerop count)
          (for-each-relevant-cons (cons index list start end from-end)
            (when (predicate (key (car cons)))
              (setf (car cons) newitem)
              (incf n-substituted)
              (when (= n-substituted count)
                (return-from nsubstitute-if list)))))
        list))))

(seal-domain #'nsubstitute-if '(t t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod nsubstitute-if (newitem predicate (vector #1#)
                             &key from-end (start 0) end count key)
    (let ((count (canonicalize-count count))
          (n-substituted 0))
      (with-predicate (predicate predicate)
        (with-key-function (key key)
          (unless (zerop count)
            (for-each-relevant-element (element index vector start end from-end)
              (when (predicate (key element))
                (setf element newitem)
                (incf n-substituted)
                (when (= n-substituted count)
                  (return-from nsubstitute-if vector)))))
          vector)))))

(seal-domain #'nsubstitute-if '(t t vector))
