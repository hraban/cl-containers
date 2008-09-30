(in-package #:containers)

;;; persistent-sorted-list-container

(defclass* persistent-sorted-list-container (sorted-container-mixin
                                             list-container concrete-container)
  "A list container that keeps its items sorted as needed. This uses pushnew-ordered."
  ()
  (:export-p t))


(defmethod insert-list ((container persistent-sorted-list-container) (list t))
  (loop for item in list do
        (insert-item container item)))


(defmethod insert-item ((container persistent-sorted-list-container) (item t))
  (pushnew-ordered
   item (slot-value container 'contents) (sorter container)
   :test (test container) :key (key container))
  (values item))


(defmethod update-item ((container persistent-sorted-list-container) (item t))
  (delete-item container item)
  (insert-item container item))


(defmethod ensure-sorted ((container persistent-sorted-list-container))
  container)

(defmethod force-sort ((container persistent-sorted-list-container))
  container)