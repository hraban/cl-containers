(in-package #:cl-containers)

(defcondition container-condition () 
    ((container :initarg :container :reader container)))

(defcondition container-error (container-condition error)
  ()
  (:export-p t)
  (:export-slots-p t))

(defcondition index-out-of-range-error (container-error)
  ((index 0 ir))
  (:report (lambda (c s)
             (if (< (index c) 0)
               (format s "Index ~D is negative, it must be between 0 and the size of the container."
                       (index c))
               (format s "Index ~D out of range for container ~A, size is only ~D."
                       (index c) (class-name (class-of (container c))) (size (container c)))))))

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
