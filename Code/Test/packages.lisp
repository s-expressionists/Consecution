(cl:in-package #:common-lisp-user)

(defpackage #:consecution-test
  (:use #:common-lisp #:regression-test)
  (:shadow #:handler-case #:handler-bind)
  (:shadowing-import-from
   #:consecution
   . #.(loop for symbol being the external-symbols of '#:consecution
             collect symbol)))
