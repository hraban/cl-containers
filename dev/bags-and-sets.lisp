(in-package #:containers)

;;; Bags
;;;
;;; Support: insert-item, delete-item, search-for-item, size, empty-p, empty!
;;; iterate-nodes

(defclass* abstract-bag/set-container (uses-contents-mixin 
                                         findable-container-mixin
                                         unordered-container-mixin
                                         iteratable-container-mixin
                                         initial-contents-mixin)
  ())


(defmethod make-container-for-contents ((container abstract-bag/set-container)
                                        &rest args)
  (apply #'make-container 'bag/set-container args))


(defclass* bag-container (abstract-bag/set-container
                          concrete-container)
  ((contents :reader contents)))


(defmethod insert-item ((container bag-container) item)
  (insert-item (contents container) item))

(defmethod size ((container abstract-bag/set-container))
  (size (contents container)))

(defmethod empty-p ((container abstract-bag/set-container))
  (empty-p (contents container)))

(defmethod empty! ((container abstract-bag/set-container))
  (empty! (contents container))
  (values))

(defmethod search-for-item ((container abstract-bag/set-container) item &key 
                            (test #'eq) (key #'identity))
  (search-for-item (contents container) item :test test :key key))

(defmethod search-for-match ((container abstract-bag/set-container) predicate &key 
                             (key 'identity))
  (search-for-match (contents container) predicate :key key))


(defmethod delete-item ((container abstract-bag/set-container) item)
  (when (delete-item (contents container) item)
    item))


(defmethod find-item ((container abstract-bag/set-container) item)
  (find-item (contents container) item))


;;; Sets
;;; Support: insert-item, delete-item, search-for-item, size, empty-p, empty!
;;; iterate-nodes

(defclass* set-container (abstract-bag/set-container
                          concrete-container)
  ((contents :reader contents)))


(defmethod insert-item ((container set-container) item)
  (unless (find-item (contents container) item)
    (insert-item (contents container) item))
  item)

;;; bag/set-container container

(defclass* bag/set-container (contents-as-hashtable-mixin
                                unordered-container-mixin
                                concrete-container)
  ((set-or-bag :set ir)))


(defmethod insert-item ((container bag/set-container) item)
  (multiple-value-bind (value found?)
                       (gethash item (contents container))
    (if found?
      (setf (gethash item (contents container)) 
            (if (eq (set-or-bag container) :set) 1 (incf value)))
      (setf (gethash item (contents container)) 1)))
  (values item))


(defmethod delete-item ((container bag/set-container) item)
  (multiple-value-bind (value found?)
                       (gethash item (contents container))
    (when found?
      (if (= value 1)
        (remhash item (contents container))
        (setf (gethash item (contents container)) (decf value)))
      item)))


(defmethod size ((container bag/set-container))
  (let ((result 0))
    (maphash (lambda (k v)
               (declare (ignore k))
               (incf result v))
             (contents container))
    result))


(defmethod search-for-item ((container bag/set-container) item &key 
                            (test 'eq) (key 'identity))
  (maphash (lambda (k v)
             (declare (ignore v))
             (when (funcall test item (funcall key k))
               (return-from search-for-item (values k t))))
           (contents container))
  (values nil nil))


(defmethod iterate-nodes ((container bag/set-container) fn)
  (maphash (lambda (k v)
             (loop repeat v do
                   (funcall fn k)))
           (contents container)))


(defmethod find-item ((container bag/set-container) item)
  (multiple-value-bind (value found?)
                       (gethash item (contents container))
    (declare (ignore value))
    (when found?
      item)))


(defmethod find-value ((container bag/set-container) item)
  (multiple-value-bind (value found?)
                       (gethash item (contents container))
    (when found?
      value)))

;;; keyed-bag/set-container
;;;
;;; when a hash table just won't do

(defclass* keyed-bag/set-container (bag/set-container)
  ((key-map nil r))
  (:export-p t))


(defmethod initialize-instance :after ((object keyed-bag/set-container) &key)
  (setf (slot-value object 'key-map)
        (make-hash-table :test (test object))))


(defmethod insert-item ((container keyed-bag/set-container) item)
  (let ((key (funcall (key container) item)))
    (setf (gethash key (key-map container)) item)
    (call-next-method container key))
  (values item))


(defmethod delete-item ((container keyed-bag/set-container) item)
  (call-next-method container (funcall (key container) item)))


;; weird, won't necessary find the item we ask for...
(defmethod find-item ((container keyed-bag/set-container) item)
  (let ((key (funcall (key container) item)))
    (call-next-method container key)))


(defmethod iterate-nodes ((container keyed-bag/set-container) fn)
  (maphash (lambda (k v)
             (let ((item (gethash k (key-map container))))
               (loop repeat v do
                     (funcall fn item))))
           (contents container)))