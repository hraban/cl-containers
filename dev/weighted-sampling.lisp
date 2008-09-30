
(in-package #:containers)

#|
This is the dynamic version... could make a faster static version too. One that 
you set up and then sample repeatedly. More like a random 'element' generator.
|#

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export '(weighted-sampling-container
            weight)))


(defclass* weighted-sampling-container (priority-queue-on-container)
  ((total-weight 0d0 a)
   (random-number-generator :unbound ir))
  (:default-initargs
    :random-number-generator variates:*random-generator*))


(defmethod initialize-instance :around ((object weighted-sampling-container) &rest args
                                        &key random-number-generator)
  (when random-number-generator
    (setf (slot-value object 'random-number-generator) random-number-generator))
  (remf args :random-number-generator)
  (apply #'call-next-method object args))


(defmethod element-weight ((container weighted-sampling-container) thing)
  (funcall (key container) thing))


#+Wait
(defmethod element-weight ((container weighted-sampling-container) 
                           (thing container-node-mixin))
  (element-weight container (element thing)))


(defmethod insert-item :after ((container weighted-sampling-container) thing)
  (incf (total-weight container) (element-weight container thing)))


(defmethod delete-item :after ((container weighted-sampling-container) thing)
  (decf (total-weight container) (element-weight container thing)))


(defmethod delete-node :after ((container weighted-sampling-container) 
                               (node container-node-mixin))
  (decf (total-weight container) (element-weight container (element node))))


(defmethod delete-first ((container weighted-sampling-container))
  (delete-item container (variates:next-element container)))


(defmethod variates:next-element ((container weighted-sampling-container))
  (let* ((target-weight (variates:uniform-random (random-number-generator container)
                                                 0d0 (total-weight container)))
         (current-weight 0d0)
         (element (block find-element
                    (iterate-elements 
                     container
                     (lambda (item)
                       (when (>= (incf current-weight (element-weight container item))
                                 target-weight)
                         (return-from find-element item)))))))
    (values element)))


#+Test
(u:timeit (:report t)
        (loop repeat 100 collect
              (let ((q (make-container 'weighted-sampling-container 
                                       :initial-contents '(1 5 2 2 10))))
                (dequeue q))))

#+Test
(u:timeit (:report t)
          (let ((q (make-container 'weighted-sampling-container 
                         :initial-contents '(1 5 2 2 10))))
            (loop repeat 100 collect
                  (variates:next-element q))))
