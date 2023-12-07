(cl:in-package #:consecution)

(defparameter *delete-duplicates-hash-threshold* 27)

(defmethod delete-duplicates (sequence &key from-end test test-not start end key)
  (declare (ignore from-end test test-not start end key))
  (error 'type-error :datum sequence :expected-type 'sequence))

(defmethod delete-duplicates ((list list) &key from-end test test-not (start 0) end key)
  (with-list-start-and-end (start end) (list start end)
    (let* ((amount (- end start))
           (sentinel (cons nil list))
           (previous (nthcdr start sentinel))
           (tail (nthcdr (- end start) (cdr previous))))
      (declare (dynamic-extent sentinel))
      (if (and (> amount *delete-duplicates-hash-threshold*)
               (or (and (null test) (null test-not))
                   (hash-table-test-p test)))
          ;; An O(n) algorithm using a hash table.
          (let ((hash-table (make-hash-table :test (or test #'eql) :size amount)))
            (with-key-function (key key)
              (do ((current (cdr previous) (cdr current)))
                  ((eq current tail))
                (incf (gethash (key (car current)) hash-table 0)))
              (do ((current (cdr previous) (cdr current)))
                  ((eq current tail))
                (let ((key (key (car current))))
                  (if (if (not from-end)
                          (eql (decf (gethash key hash-table)) 0)
                          (shiftf (gethash key hash-table) nil))
                      (setf previous current)
                      (setf (cdr previous) (cdr current)))))))
          ;; The default O(n^2) algorithm.
          (with-key-function (key key)
            (with-test-function (test test test-not)
              (if (not from-end)
                  (do ((current (cdr previous) (cdr current)))
                      ((eq current tail))
                    (let ((item (key (car current))))
                      (if (loop for peek = (cdr current) then (cdr peek)
                                until (eq peek tail)
                                  thereis (test item (key (car peek))))
                          (setf (cdr previous) (cdr current))
                          (setf previous current))))
                  (do ((current (cdr previous) (cdr current)))
                      ((eq current tail))
                    (let ((item (key (car current)))
                          (cons current)) ; The cons cell right before PEEK.
                      (do ((peek (cdr current) (cdr peek)))
                          ((eq peek tail))
                        (if (test item (key (car peek)))
                            (setf (cdr cons) (cdr peek))
                            (setf cons peek)))))))))
      (cdr sentinel))))

(seal-domain #'delete-duplicates '(list))

(replicate-for-each-vector-class #1=#:vector-class
  (defmethod delete-duplicates ((vector #1#) &key from-end test test-not (start 0) end key)
    (declare (type #1# vector))
    (with-vector-start-and-end (start end length) (vector start end)
      (let ((amount (- end start))
            (destination start))
        (declare (vector-length destination))
        (if (and (> amount *delete-duplicates-hash-threshold*)
                 (or (and (null test) (null test-not))
                     (hash-table-test-p test)))
            ;; An O(n) algorithm using a hash table.
            (let ((hash-table (make-hash-table :test (or test #'eql) :size (- end start))))
              (with-key-function (key key)
                (loop for index fixnum from start below end
                      for key = (key (elt vector index))
                      do (incf (gethash key hash-table 0)))
                (loop for index fixnum from start below end
                      for element = (elt vector index)
                      for key = (key element)
                      when (if (not from-end)
                               (eql (decf (gethash key hash-table)) 0)
                               (shiftf (gethash key hash-table) nil))
                        do (setf (elt vector destination) element)
                           (incf destination))))
            ;; The default O(n^2) algorithm.
            (with-test-function (test test test-not)
              (with-key-function (key key)
                (if (not from-end)
                    (loop for index fixnum from start below end do
                      (let* ((element (elt vector index))
                             (key (key element)))
                        (setf (elt vector destination) element)
                        (unless (loop for pos fixnum from (1+ index) below end
                                        thereis (test key (key (elt vector pos))))
                          (incf destination))))
                    (loop for index fixnum from start below end do
                      (let* ((element (elt vector index))
                             (key (key element)))
                        (setf (elt vector destination) element)
                        (unless (loop for pos from start below destination
                                        thereis (test (key (elt vector pos)) key))
                          (incf destination)))))
                (loop for index fixnum from end below length do
                  (setf (elt vector destination)
                        (elt vector index))
                  (incf destination))
                (shrink-vector vector destination))))))))

(seal-domain #'delete-duplicates '(vector))
