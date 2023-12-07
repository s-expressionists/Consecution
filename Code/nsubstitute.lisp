(cl:in-package #:consecution)

(defmethod nsubstitute (newitem olditem sequence
                        &key from-end test test-not start end count key)
  (declare (ignore newitem olditem from-end test test-not start end count key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod nsubstitute (newitem olditem (list list)
                        &key from-end test test-not (start 0) end count key)
  (let ((count (canonicalize-count count))
        (n-substituted 0))
    (with-test-function (test test test-not)
      (with-key-function (key key)
        (unless (zerop count)
          (for-each-relevant-cons (cons index list start end from-end)
            (when (test olditem (key (car cons)))
              (setf (car cons) newitem)
              (incf n-substituted)
              (when (= n-substituted count)
                (return-from nsubstitute list)))))
        list))))

(seal-domain #'nsubstitute '(t t list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod nsubstitute (newitem olditem (vector #1#)
                          &key from-end test test-not (start 0) end count key)
    (let ((count (canonicalize-count count))
          (n-substituted 0))
      (with-test-function (test test test-not)
        (with-key-function (key key)
          (unless (zerop count)
            (for-each-relevant-element (element index vector start end from-end)
              (when (test olditem (key element))
                (setf element newitem)
                (incf n-substituted)
                (when (= n-substituted count)
                  (return-from nsubstitute vector)))))
          vector)))))

(seal-domain #'nsubstitute '(t t vector))
