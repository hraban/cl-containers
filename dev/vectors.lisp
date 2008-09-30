
(in-package #:containers)

;;; vector-container-mixin

(defclass* vector-container-mixin (contents-as-array-mixin
                                     initial-contents-mixin
                                     ordered-container-mixin
                                     typed-container-mixin
                                     abstract-container)
  ((contents)))


(defmethod item-at ((container vector-container-mixin) &rest indices)
  (declare (dynamic-extent indices))
  (aref (contents container) (first indices)))


(defmethod item-at! ((container vector-container-mixin) value &rest indices)
  (declare (dynamic-extent indices))
  (setf (aref (contents container) (first indices))
        value))


(defmethod nth-element ((container vector-container-mixin) (index integer))
  (aref (contents container) index))


(defmethod first-element ((container vector-container-mixin))
  (item-at container 0))


(defmethod (setf first-element) (value (container vector-container-mixin))
  (setf (item-at container 0) value))


(defmethod last-element ((v vector-container-mixin))
  (item-at v (1- (size v))))


(defmethod (setf last-element) (value (v vector-container-mixin))
  (setf (item-at v (1- (size v))) value))

;;; basic-vector-container

(defclass* basic-vector-container (vector-container-mixin)
  ())


;;; bounded-vector-container

(defclass* bounded-vector-container (basic-vector-container
                                       concrete-container)
  ())


(defmethod initialize-instance :around ((object bounded-vector-container) &rest args
                                        &key size initial-size)
  (remf args :size)
  (apply #'call-next-method object :initial-size (or size initial-size) args))



;;; vector-container

;; implements size, empty-p, empty!,
;;            insert-item, delete-item,
;;            insert-item-at, delete-item-at,
;;            item-at, iterate-container, delete-first, delete-last
;;            first-element, search-for-item, search-for-match

(defclass* vector-container (basic-vector-container
			     concrete-container)
  ())


(defmethod insert-item ((container vector-container) item)
  (vector-push-extend item (contents container))
  container)


(defmethod insert-item-at ((container vector-container) item index)
  (resize-vector container (max (1+ (size container))
                                (1+ index)))
  (replace (contents container) (contents container)
           :start1 (+ 1 index)
           :start2 index
           :end2 (- (size container) 1))
  (setf (aref (contents container) index) item)
  
  container)


(defmethod delete-item-at ((container vector-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((index (car indexes)))
    (replace (contents container)
             (contents container)
             :start2 (+ 1 index)
             :start1 index)
    (decf (fill-pointer (contents container))))
  container)


(defmethod delete-item ((container vector-container) item)
  ;;; removes the first instance of item in the-vector
  (let ((position (position item (contents container))))
    (when position
      (delete-item-at container position))
    container))

#+Wrong
;;?? This doesn't keep the adjustable property on all lisps
(defmethod delete-item ((container vector-container) item)
  ;;; removes the first instance of item in the-vector
  (setf (contents container) (delete item (contents container) :count 1)))

#+Wrong
;;?? This is fast but doesn't respect the ordered-container contract
(defmethod delete-item ((container vector-container) item)
  (let ((position (position item (contents container))))
    (when position
      (rotatef (aref (contents container) position)
	       (aref (contents container) (1- (length (contents container)))))
      (setf (fill-pointer (contents container))
	    (1- (length (contents container)))))
    container))


(defmethod delete-first ((v vector-container))
  (prog1
    (item-at v 0)
    (delete-item-at v 0)))


(defmethod delete-last ((v vector-container))
  (let ((index (1- (size v))))
    (prog1 
      (item-at v index)
      (delete-item-at v index))))


(defmethod size ((v vector-container))
  (length (contents v)))



;;; flexible-vector-container

(defclass* flexible-vector-container (vector-container)
  ())


(defmethod item-at! :before 
    ((container flexible-vector-container) value &rest indices)
  (declare (ignore value)
           (dynamic-extent indices))
  (resize-vector container (1+ (first indices))))


#+Vector-Test
(let ((vec (make-container 'vector-container)))
  (size vec)
  (empty-p vec)
  (insert-item vec 'a)
  (size vec)
  (item-at vec 0)
  (insert-item-at vec 'b 0)
  (empty-p vec)
  (delete-item vec 'b)
  (size vec)
  (delete-item-at vec 0))

;;; utilities

(defun resize-vector (vector new-size)
  (unless (= (size vector) new-size)
    (setf (contents vector)
          (adjust-array (contents vector) 
                        (max (size vector) new-size)
                        :element-type 
                        (array-element-type (contents vector))
                        :fill-pointer (array-has-fill-pointer-p (contents vector))))))


(defmethod samep ((container-1 vector-container-mixin) 
                  (container-2 vector-container-mixin))
  (let ((*samep-test* #'equalp))
    (samep (contents container-1) (contents container-2))))