(in-package #:cl-containers)

(defcondition container-condition () 
    ((container :initarg :container :reader container)))

(defcondition container-error (container-condition error)
  ()
  (:export-p t)
  (:export-slots-p t))

(define-condition key-does-not-exist-error (container-error)
  ())

(define-condition queue-empty (container-error)
                  ((message :initarg :message
                            :reader message))
  (:report (lambda (c stream)
             (format stream "~A" (message c)))))

(define-condition record-not-found-error (container-error)
                  ((table :initarg :table :accessor table)
                   (value :initarg :value :accessor value)))

(defcondition element-not-found-error (container-error)
    ((element :initarg :element :reader element))
    (:export-p t)
  (:export-slots-p element))
