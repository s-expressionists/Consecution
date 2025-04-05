(cl:in-package #:consecution)

(defclass sequence-function (fast-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class))

(defmethod no-applicable-method
    ((sequence-function sequence-function) &rest arguments)
  (maybe-signal-sequence-type-error sequence-function arguments)
  (call-next-method))

(defmethod no-primary-method
    ((sequence-function sequence-function) &rest arguments)
  (maybe-signal-sequence-type-error sequence-function arguments)
  (call-next-method))

(defun maybe-signal-sequence-type-error (sequence-function arguments)
  (dolist (sealed-domain (sealed-domains sequence-function))
    (loop for specializer in (sealable-metaobjects:domain-specializers sealed-domain)
          for argument in arguments
          when (subtypep specializer 'sequence)
            unless (typep argument 'sequence)
              do (error 'type-error
                        :expected-type 'sequence
                        :datum argument))))

(defmacro define-sequence-function (name lambda-list)
  `(defgeneric ,name ,lambda-list
     (:generic-function-class sequence-function)))
