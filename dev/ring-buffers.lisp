(in-package #:containers)

;;; Ring Buffers
;;;
;;; Code adapted from ANSI Common Lisp by Paul Graham (chapter 7)
;;;
;;; A ring buffer is a bounded queue. It supports:
;;;   item-at (setf item-at)
;;;   insert-item, dequeue, empty!, empty-p, size, total-size, first-element

(defclass* ring-buffer (abstract-queue
                          bounded-container-mixin
                          iteratable-container-mixin
                          concrete-container)
  ((contents :initarg :contents
             :reader contents)
   (buffer-start :initform 0
                 :reader buffer-start)
   (buffer-end :initform 0
               :reader buffer-end)
   (total-size :initarg :total-size
               :reader total-size)))

(defun make-ring-buffer (size)
  (make-instance 'ring-buffer
    :contents (make-array size)
    :total-size size))

(defmethod make-container ((class (eql 'ring-buffer)) &rest args)
   (let ((total-size (getf args :total-size 1)))
     (remf args :total-size)
     (make-ring-buffer total-size)))


;;?? the (first indexes) is odd...
(defmethod item-at ((container ring-buffer) &rest indexes)
   (declare (dynamic-extent indexes))
   (svref (contents container)
          (mod (first indexes) (total-size container))))

(defmethod item-at! ((container ring-buffer) value &rest indexes)
  (declare (dynamic-extent indexes))
  (setf (svref (contents container)
                (mod (first indexes) (total-size container)))
         value))


(defmethod increment-end ((container ring-buffer))
   (with-slots (buffer-end buffer-start) container
     (when (and (>= buffer-end (total-size container))
                (= (mod buffer-end (total-size container)) buffer-start))
       (incf buffer-start))
     (incf buffer-end)))


(defmethod next-item ((container ring-buffer))
  (increment-end container)
  (current-item container))


(defmethod current-item ((container ring-buffer))
  (item-at container (buffer-end container)))


(defmethod insert-item ((container ring-buffer) item)
   (prog1
     (setf (item-at container (buffer-end container)) item)
     (increment-end container)))


(defmethod delete-first ((container ring-buffer))
   (with-slots (buffer-start) container
     (prog1
       (item-at container buffer-start)
       (incf buffer-start))))


(defmethod empty! ((container ring-buffer))
   (with-slots (buffer-end buffer-start)
               container
     (setf buffer-start 0
           buffer-end 0))

   (values))


#+Ignore
(defmethod total-size ((container ring-buffer))
   (total-size container))


(defmethod size ((container ring-buffer))
   (- (buffer-end container) (buffer-start container)))


(defmethod first-element ((container ring-buffer))
   (item-at container (buffer-start container)))


(defmethod (setf first-element) (value (container ring-buffer))
   (setf (item-at container (buffer-start container)) value))


(defmethod iterate-nodes ((container ring-buffer) fn)
   (loop for index from (buffer-start container) to (1- (buffer-end container)) do
         (funcall fn (item-at container index))))

#+No
;; screws with the buffer pointers
(defmethod iterate-nodes ((container ring-buffer) fn)
  (loop repeat (total-size container)
        with item = (current-item container) do
        (funcall fn item)
        (setf item (next-item container))))


