(cl:in-package #:consecution)

(define-sequence-function copy-seq
    (sequence))

(define-sequence-function elt
    (sequence index))

(define-sequence-function (setf elt)
    (value sequence index))

(define-sequence-function fill
    (sequence item &key start end))

(define-sequence-function make-sequence-like
    (prototype length &key initial-element initial-contents))

(define-sequence-function adjust-sequence
    (sequence length &key initial-element initial-contents))

(define-sequence-function subseq
    (sequence start &optional end))

(define-sequence-function map-into
    (result-sequence function &rest sequences))

(define-sequence-function reduce
    (function sequence &key key from-end start end initial-value))

(define-sequence-function count
    (item sequence &key from-end start end key test test-not))

(define-sequence-function count-if
    (predicate sequence &key from-end start end key))

(define-sequence-function count-if-not
    (predicate sequence &key from-end start end key))

(define-sequence-function length
    (sequence))

(define-sequence-function reverse
    (sequence))

(define-sequence-function nreverse
    (sequence))

(define-sequence-function sort
    (sequence predicate &key key))

(define-sequence-function stable-sort
    (sequence predicate &key key))

(define-sequence-function find
    (item sequence &key from-end test test-not start end key))

(define-sequence-function find-if
    (predicate sequence &key from-end start end key))

(define-sequence-function find-if-not
    (predicate sequence &key from-end start end key))

(define-sequence-function position
    (item sequence &key from-end test test-not start end key))

(define-sequence-function position-if
    (predicate sequence &key from-end start end key))

(define-sequence-function position-if-not
    (predicate sequence &key from-end start end key))

(define-sequence-function search
    (sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2))

(define-sequence-function mismatch
    (sequence-1 sequence-2 &key from-end test test-not key start1 start2 end1 end2))

(define-sequence-function replace
    (sequence-1 sequence-2 &key start1 end1 start2 end2))

(define-sequence-function substitute
    (newitem olditem sequence &key from-end test test-not start end count key))

(define-sequence-function substitute-if
    (newitem predicate sequence &key from-end start end count key))

(define-sequence-function substitute-if-not
    (newitem predicate sequence &key from-end start end count key))

(define-sequence-function nsubstitute
    (newitem olditem sequence &key from-end test test-not start end count key))

(define-sequence-function nsubstitute-if
    (newitem predicate sequence &key from-end start end count key))

(define-sequence-function nsubstitute-if-not
    (newitem predicate sequence &key from-end start end count key))

(define-sequence-function remove
    (item sequence &key from-end test test-not start end count key))

(define-sequence-function remove-if
    (test sequence &key from-end start end count key))

(define-sequence-function remove-if-not
    (test sequence &key from-end start end count key))

(define-sequence-function delete
    (item sequence &key from-end test test-not start end count key))

(define-sequence-function delete-if
    (test sequence &key from-end start end count key))

(define-sequence-function delete-if-not
    (test sequence &key from-end start end count key))

(define-sequence-function remove-duplicates
    (sequence &key from-end test test-not start end key))

(define-sequence-function delete-duplicates
    (sequence &key from-end test test-not start end key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Auxiliary Functions

(define-sequence-function make-sequence-scanner (sequence))

(define-sequence-function
    concatenate-sequence-like (prototype &rest sequences))

(define-sequence-function merge-sequence-like
    (prototype sequence-1 sequence-2 predicate &key key))

(define-sequence-function make-sequence-reader
    (sequence start end from-end terminate))

(define-sequence-function make-sequence-writer
    (sequence start end from-end terminate))
