
(in-package #:containers)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package (find-package 'cl-variates) 
               (find-package 'cl-containers)))


(defmethod sample-item ((container t) 
                        (generator variates:random-number-generation-mixin))
  (sample-item container (variates:random-number-generator generator)))


(defmethod sample-item ((container iteratable-container-mixin) 
                        (generator variates:basic-random-number-generator))
  (let ((index (variates:integer-random generator 0 (1- (size container)))))
    (nth-item container index)))


(defmethod sample-item ((container sequence) 
                        (generator variates:basic-random-number-generator))
  (declare (inline sample-element))
  (sample-element container generator))


(defmethod nth-item ((container iteratable-container-mixin) index)
  ;; possibly slow but servicable method
  (iterate-container
   container
   (lambda (elt)
     (when (minusp (decf index))
       (return-from nth-item elt))))
  (error "Index ~D out of range for container ~A" index container))


(defmethod sample-element ((container t) 
                           (generator variates:random-number-generation-mixin))
  (sample-element container (variates:random-number-generator generator)))


(defmethod sample-element ((container (eql nil)) 
                           (generator variates:basic-random-number-generator))
  (values nil))


(defmethod sample-element ((container iteratable-container-mixin) 
                           (generator variates:basic-random-number-generator))
  (let ((element (variates:integer-random generator 0 (1- (size container)))))
    (nth-element container element)))


(defmethod sample-element ((container sequence) 
                           (generator variates:basic-random-number-generator))
  (let ((element (variates:integer-random generator 0 (1- (size container)))))
    (nth-element container element)))


(defmethod sample-unique-elements ((container t) 
                                   (generator variates:random-number-generation-mixin)
                                   (count integer))
  (sample-unique-elements
   container (variates:random-number-generator generator) count))

(defmethod sample-unique-elements ((container iteratable-container-mixin) 
                                   (generator variates:basic-random-number-generator)
                                   (count integer))
  (%sample-unique-elements container generator count)
  #+Old
  (loop for bit across (variates:select-sample generator count (size container))
        for index = 0 then (1+ index) 
        unless (zerop bit) collect
        (nth-element container index)))


(defmethod sample-unique-elements ((container list) 
                                   (generator variates:basic-random-number-generator)
                                   (count integer))
  (%sample-unique-elements container generator count)
  #+Old
  (loop for bit across (variates:select-sample generator count (size container))
        for index = 0 then (1+ index) 
        unless (zerop bit) collect
        (nth-element container index)))


(defmethod sample-unique-elements ((container array) 
                                   (generator variates:basic-random-number-generator)
                                   (count integer))
  (%sample-unique-elements container generator count)
  #+Old
  (loop for bit across (variates:select-sample generator count (size container))
        for index = 0 then (1+ index) 
        unless (zerop bit) collect
        (nth-element container index)))


#+SLOWER
;;?? Gary King 2005-11-04: intuitively, this should win because it bails out 
;; early. However, loop must be doing something clever and so it doesn't.
(defun %sample-unique-elements (container generator count)
  (let ((result nil))
    (loop for bit across (variates:select-sample generator count (size container))
          for index = 0 then (1+ index) 
          unless (zerop bit) do
          (decf count)
          (push (nth-element container index) result)
          when (zerop count) do
          (return))
    (nreverse result)))

(defun %sample-unique-elements (container generator count)
  (let ((result nil))
    (loop for bit across (variates:select-sample generator count (size container))
          for index = 0 then (1+ index) 
          unless (zerop bit) do
          (push (nth-element container index) result)
          )
    (nreverse result)))

#+Test
(let ((l (collect-elements (make-generator :start 0 :end 1000))))
  (timeit (:report t)
          (loop repeat 1000 do
                (sample-unique-elements l *random-generator* 10))))


(defmethod sample-elements ((container t) 
                           (generator variates:random-number-generation-mixin)
                           (count integer))
  (sample-elements container (variates:random-number-generator generator) count))


(defmethod sample-elements ((container iteratable-container-mixin) 
                            (generator variates:basic-random-number-generator)
                            (count integer))
  (loop repeat count collect
        (sample-element container generator)))


(defmethod sample-elements ((container list) 
                            (generator variates:basic-random-number-generator)
                            (count integer))
  (loop repeat count collect
        (sample-element container generator)))


(defun safe-sample-unique-elements (container generator count)
  (if (length-at-most-p container count)
    (collect-elements container)
    (sample-unique-elements container generator count)))

#+Test
(let ((c (make-container 'list-container 
                         :initial-contents (loop for i from 0 to 10 collect i))))
  (containers::sample-unique-elements c variates:*random-generator* 5))

#+Test
(let ((c (make-container 'array-container :dimensions '(5 5))))
  (loop for i from 0 to 4 do
        (loop for j from 0 to 4 do
              (setf (item-at c i j) (* (1+ i) (1+ j)))))
  (containers::sample-unique-elements c variates:*random-generator* 5))


(defmethod variates:shuffle-elements! ((container abstract-container)
                                       &key (generator variates:*random-generator*) 
                                       (times 0 times-supplied?))
  (let ((size (1- (size container))))
    (dotimes (i (if times-supplied? times (* 2 size)))
      (rotatef (nth-element container (variates:integer-random generator 0 size))
               (nth-element container (variates:integer-random generator 0 size))))))


(defmethod variates:shuffle-elements! ((container uses-contents-mixin)
                                       &rest args)
  (declare (dynamic-extent args))
  (apply #'variates:shuffle-elements! (contents container) args)
  container)
