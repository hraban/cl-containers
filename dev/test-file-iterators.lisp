#|

(let ((i (make-iterator 
          #P"Billy-Pilgrim:Users:gwking:repository:p2dis:lsa:dev:data:titles-1.data"
          )))
  (collect-elements i))
         

(let ((i (make-iterator 
          #P"Billy-Pilgrim:Users:gwking:repository:p2dis:lsa:dev:data:titles-1.data"
          :treat-contents-as :file-lines)))
  (collect-elements i))
         
(let ((i (make-iterator 
          "Billy-Pilgrim:Users:gwking:repository:p2dis:lsa:dev:data:titles-1.data"
          :treat-contents-as :file-lines
          :transform (lambda (line)
                       (make-iterator line :treat-contents-as :words)))))
  (iterate-elements i
                    (lambda (e)
                      (iterate-elements e #'print))))

(let ((i (make-iterator 
          "Billy-Pilgrim:Users:gwking:repository:p2dis:lsa:dev:data:titles-1.data"
          :treat-contents-as :file-lines
          :transform (lambda (line) (make-iterator line)))))
  (iterate-elements i
                    (lambda (e)
                      (iterate-elements e #'print))))

(let ((i (make-instance 'file-line-iterator
           :filename "Billy-Pilgrim:Users:gwking:repository:p2dis:lsa:dev:data:titles-1.data")))
  (iterate-forward i (lambda (e) (format t "~%L: ~A" e))))



(let ((i (make-iterator "attractors for semigroups and evolution equations")))
  (iterate-forward i #'print))

(let ((i (make-iterator "attractors for semigroups and evolution equations"
                        :treat-contents-as :words)))
  (iterate-forward i #'print))
|#



#| Necessary??

(let ((i (make-iterator "hello"))) (collect-elements i))

;;; ---------------------------------------------------------------------------
;;; character iterator
;;;
;;; UNICODE!!!
;;; ---------------------------------------------------------------------------

(defclass* character-iterator (forward-iterator)
  ((cache (make-array 20 :element-type 'character :fill-pointer 0 :adjustable t) r)
   (current-word nil r)
   (internal-iterator nil r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object character-iterator) &key container)
  (setf (slot-value object 'internal-iterator) 
        (make-iterator container))
  (when (move-forward-p (internal-iterator object))
    (move-forward (internal-iterator object)))
  (advance object))

;;; ---------------------------------------------------------------------------

(defmethod move ((iterator character-iterator) (direction (eql :forward)))
  (advance iterator))

;;; ---------------------------------------------------------------------------

(defmethod advance ((iterator character-iterator))
  (let ((internal (internal-iterator iterator)))
    (setf (fill-pointer (cache iterator)) 0) 
    (loop while (move-forward-p internal) do
          (when (delimiter-p iterator (current-element internal))
            (loop while (and (move-forward-p internal)
                             (delimiter-p iterator (current-element internal))) do
                  (move-forward internal))
            (return))
          (vector-push-extend (current-element internal) (cache iterator))
          (move-forward internal))
    (setf (slot-value iterator 'current-word)
          (coerce (cache iterator) 'string))))

;;; ---------------------------------------------------------------------------

(defmethod current-element ((iterator character-iterator))
  (current-word iterator))

;;; ---------------------------------------------------------------------------

(defmethod current-element-p ((iterator character-iterator))
  (and (call-next-method)
       (plusp (fill-pointer (cache iterator)))))

;;; ---------------------------------------------------------------------------

(defmethod move-p ((iterator character-iterator) (direction (eql :forward)))
  (or (move-p (internal-iterator iterator) direction)
      (plusp (size (cache iterator)))))

;;; ---------------------------------------------------------------------------

(defmethod class-for-contents-as ((contents string) (as (eql :words)))
  'character-iterator)
|#



#|
(setf i
      (let ((i (make-instance 'word-iterator
                 :container
                 "attractors and blood"
                 #+Ignore
                 "Billy-Pilgrim:Users:gwking:repository:p2dis:lsa:dev:data:titles-1.data")))
        (iterate-forward i (lambda (e) (format t "~%L: ~A" e)))
        i))

(setf i
      (let ((i (make-iterator "hello")))
        (iterate-forward i (lambda (e) (format t "~%L: ~A" e)))
        i))


(setf i (make-instance 'word-iterator
           :container "attractors for semigroups and evolution equations"))

(current-element-p i)
(move-forward-p i)
(Move-forward i)
(element i)

(setf i (make-iterator "attractors    and blood" :treat-contents-as :words))
(iterate-elements i #'print)
(collect-elements (make-iterator "attractors    and blood" :treat-contents-as :words))
|#