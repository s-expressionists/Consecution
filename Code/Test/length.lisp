;-*- Mode:     Lisp -*-
;;;; Author:   Paul Dietz
;;;; Created:  Tue Aug 20 23:25:29 2002
;;;; Contains: Test cases for LENGTH

(in-package #:consecution-test)

(deftest length.list.1
  (length nil)
  0)

(deftest length.list.2
  (length '(a b c d e))
  5)

(deftest length.list.3
    (length (make-list 200000))
  200000)

(defun length.list-4-body ()
  (let ((x ()))
    (loop
     for i from 0 to 999 do
     (progn
       (unless (eql (length x) i) (return nil))
       (push i x))
     finally (return t))))

(deftest length.list-4
  (length.list-4-body)
  t)

(deftest length.vector.1
  (length #())
  0)

(deftest length.vector.2
  (length #(a))
  1)

(deftest length.vector.3
  (length #(a b))
  2)

(deftest length.vector.4
  (length #(a b c))
  3)

(deftest length.nonsimple-vector.1
  (length (make-array 10 :fill-pointer t :adjustable t))
  10)

(deftest length.nonsimple-vector.2
  (let ((a (make-array 10 :fill-pointer t :adjustable t)))
    (setf (fill-pointer a) 5)
    (length a))
  5)

`(deftest length.bit-vector.1
  (length #*)
  0)

(deftest length.bit-vector.2
  (length #*1)
  1)

(deftest length.bit-vector.3
  (length #*0)
  1)

(deftest length.bit-vector.4
  (length #*010101)
  6)

(deftest length.bit-vector.5
  (let ((i 0))
    (flet ((%f () (incf i)
               (make-array 5 :element-type 'bit
                           :initial-contents '(0 0 1 1 0))))
      (values
       (length (the (simple-bit-vector 5) (%f)))
       i)))
  5 1)

(deftest length.string.1
  (length "")
  0)

(deftest length.string.2
  (length "a")
  1)

(deftest length.string.3
  (length "abcdefghijklm")
  13)

(deftest length.string.4
  (length "\ ")
  1)

(deftest length.string.5
  (let ((i 0))
    (flet ((%f () (incf i)
               (make-string 5 :initial-element #\a)))
      (values (length (the (simple-string 5) (%f))) i)))
  5 1)

(deftest length.string.6
  (let ((i 0))
    (flet ((%f () (incf i)
               (make-array 5 :element-type 'base-char
                           :initial-element #\a)))
      (values (length (the (simple-base-string 5) (%f))) i)))
  5 1)

(deftest length.string.7
  (do-special-strings
   (s "12345" nil)
   (assert (= (length s) 5)))
  nil)

(deftest length.string.8
  (do-special-strings
   (s "" nil)
   (assert (= (length s) 0)))
  nil)

;;; Error cases

(deftest length.error.1
  (check-type-error #'length #'(lambda (x) (typep x 'sequence)))
  nil)

(deftest length.error.6
  (signals-error (length) program-error)
  t)

(deftest length.error.7
  (signals-error (length nil nil) program-error)
  t)

(deftest length.error.8
  (signals-error (locally (length 'a) t) type-error)
  t)

;;; Length on vectors created with make-array

(deftest length.array.1
  (length (make-array '(20)))
  20)

(deftest length.array.2
  (length (make-array '(100001)))
  100001)

(deftest length.array.3
  (length (make-array '(0)))
  0)

(deftest length.array.4
  (let ((x (make-array '(100) :fill-pointer 10)))
    (length x))
  10)

(deftest length.array.5
  (let ((x (make-array '(100) :fill-pointer 10)))
    (setf (fill-pointer x) 20)
    (length x))
  20)

;;; Unusual vectors

(deftest length.array.6
  (loop for i from 1 to 40
        for etype = `(unsigned-byte ,i)
        for vec = (make-array 7 :element-type etype :initial-element 0)
        for len = (length vec)
        unless (eql len 7)
        collect (list i vec len))
  nil)

(deftest length.array.7
  (loop for i from 1 to 40
        for etype = `(signed-byte ,i)
        for vec = (make-array 13 :element-type etype :initial-element 0)
        for len = (length vec)
        unless (eql len 13)
        collect (list i vec len))
  nil)

(deftest length.array.8
  (loop for etype in '(short-float single-float double-float long-float rational)
        for vec = (make-array 5 :element-type etype :initial-element (coerce 0 etype))
        for len = (length vec)
        unless (eql len 5)
        collect (list etype vec len))
  nil)

(deftest length.array.9
  (do-special-integer-vectors
   (v #(0 1 1 0 0 1) nil)
   (assert (eql (length v) 6)))
  nil)
