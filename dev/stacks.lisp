(in-package #:containers)

;;; Abstract Stack interface
;;;
;;; Basic and much loved first-in-last-out container.
;;;
;;; Supports:
;;; insert-item, size, empty-p, empty!, first-element,
;;; pop-item, push-item (= insert-item)

(defclass* abstract-stack (initial-contents-mixin
                           iteratable-container-mixin
                           ordered-container-mixin)
  ())

(defmethod (setf first-element) (value (stack abstract-stack))
  ;;?? should this fail when stack is empty
  (pop-item stack)
  (insert-item stack value))

(defmethod push-item ((stack abstract-stack) item)
  (insert-item stack item))


;;; Stack

(defclass* stack-container (uses-contents-mixin abstract-stack concrete-container)
  ((contents :unbound r)
   (container-type nil ir))
  (:default-initargs
    :container-type 'list-container))


(defmethod make-container-for-contents ((container stack-container)
                                        &rest args)
  (apply #'make-container (container-type container) args))


(defmethod first-element ((container stack-container))
  (first-element (contents container)))


(defmethod (setf first-element) (value (container stack-container))
  (setf (first-element (contents container)) value))


(defmethod pop-item ((container stack-container))
  (delete-first (contents container)))

(defmethod insert-item ((container stack-container) item)
  (insert-item (contents container) item))

(defmethod size ((container stack-container))
  (size (contents container)))

(defmethod empty-p ((container stack-container))
  (empty-p (contents container)))

(defmethod empty! ((container stack-container))
  (empty! (contents container))
  (values))

(defmethod search-for-item ((container stack-container) item &key 
                            (test 'eq) (key 'identity))
  (search-for-item (contents container) item :test test :key key))