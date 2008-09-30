(in-package #:containers)

;;; Heap based queue using EKSL priority-queue
;;?? Needs a better name

#+EKSL-PRIORITY-QUEUE
(progn
  (defclass* priority-queue-heap (abstract-queue concrete-container
                                                 iteratable-container-mixin)
    ((contents  :initform nil
                :initarg :contents
                :accessor contents)))
  
  (defmethod make-container ((class (eql 'priority-queue-heap)) &rest args)
    (make-instance 'priority-queue-heap
      :contents (apply #'make-sorted-queue args)))
  
  (defun make-sorted-queue (&key (not-lessp-predicate #'<)
                                 (key #'identity)
                                 (initial-length 42))
    (u:make-priority-queue
     not-lessp-predicate
     :key key
     :size initial-length))
  
  (defmethod insert-item ((container priority-queue-heap) item)
    (u:priority-queue-insert item (contents container)))
  
  (defmethod find-item ((container priority-queue-heap) item)
    (u:priority-queue-find item (contents container)))
  
  (defmethod search-for-item ((container priority-queue-heap) item &key test key)
    (u:priority-queue-find item (contents container) :key key :test test))
  
  (defmethod delete-item ((container priority-queue-heap) item)
    (u:priority-queue-delete item (contents container)))
  
  (defmethod delete-first ((container priority-queue-heap))
    (u:priority-queue-pop (contents container)))
  
  ;; Should not be called on an empty queue... results might be undefined.
  (defmethod first-item ((container priority-queue-heap))
    (u:priority-queue-head (contents container)))
  
  (defmethod empty-p ((container priority-queue-heap))
    (u:priority-queue-empty-p (contents container)))
  
  (defmethod empty! ((container priority-queue-heap))
    (u:priority-queue-empty (contents container))
    (values))
  
  (defmethod iterate-nodes ((container priority-queue-heap) fn)
    (u:priority-queue-map-in-priority-order
     (lambda (element index)
       (declare (ignore index))
       (funcall fn element))
     (contents container)))
  
  (defmethod size ((container priority-queue-heap))
    (u:priority-queue-length (contents container)))
  
  (u:defcopy-methods priority-queue-heap :copy-all t)
  (u:make-load-form* priority-queue-heap))
