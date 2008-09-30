(in-package #:containers)


(defclass* package-container (iteratable-container-mixin
                                abstract-container)
  ((packages nil ir)
   (exported-symbols-only-p t ia)       ; :external
   (present-symbols-only-p t ia)        ; :internal or :external
   ))


(defmethod (setf packages) ((value symbol) (container package-container))
  (setf (packages container) (list value)))


(defmethod (setf packages) ((value cons) (container package-container))
  (assert (every-element-p value (lambda (e) (find-package e))))
  (setf (slot-value container 'packages) value))


(defmethod iterate-elements ((container package-container) fn)
  (block iterator
    (with-package-iterator (x (packages container)
			      :internal :external :inherited)
      (loop
        (multiple-value-bind (more? symbol type) (x)
          (unless more? (return-from iterator))
          (when (or (and (exported-symbols-only-p container) 
			 (eq type :external))
                    (and (not (exported-symbols-only-p container))
			 (present-symbols-only-p container) 
                         (eq type :internal))
                    (and (not (present-symbols-only-p container))
                         (not (exported-symbols-only-p container))
                         (eq type :inherited))) 
            (funcall fn symbol)))))))

(defmethod size ((container package-container))
  ;; it's gonna cons
  (count-using #'iterate-elements nil container))

#+test
(iterate-elements
 (make-container 'package-container :packages (list :p2dis) :present-symbols-only-p t
                 :exported-symbols-only-p nil)
 #'print) 


(defun bound-symbols-in-package (package)
  (iterate-elements
   (make-container 'package-container 
                   :packages (list package)
                   :present-symbols-only-p t
                   :exported-symbols-only-p nil)
   (lambda (s)
     (when (and (boundp s) (symbol-value s)) (print s)))))

#+Test
(bound-symbols-in-package 'p2dis)
  