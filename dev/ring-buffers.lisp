(in-package #:containers)

;;; Ring Buffers
;;;
;;; Code adapted from ANSI Common Lisp by Paul Graham (chapter 7)
;;;
;;; A ring buffer is a bounded queue. It supports:
;;;   item-at
;;;   item-at! (setf item-at)
;;;   insert-item, empty!, empty-p, size, total-size,
;;;   dequeue (delete-first)
;;;   delete-last
;;;   delete-item
;;;   delete-item-if
;;;   first-item (first-element)
;;;   last-item (last-element)
;;;   container->list
;;;   iterate-nodes
;;;   element-position

(defclass* ring-buffer (abstract-queue
                        bounded-container-mixin
                        iteratable-container-mixin
                        concrete-container)
  ((contents :reader contents
             :initarg :contents)
   (buffer-start :reader buffer-start
                 :initform 0)
   (buffer-end :reader buffer-end
               :initform 0)
   (total-size :reader total-size
               :initarg :total-size)))

(defclass* ring-buffer-reverse (ring-buffer) ())

(defun make-ring-buffer (size &optional last-in-first-out)
  (make-instance (if last-in-first-out 'ring-buffer-reverse 'ring-buffer)
                 :contents (make-array size)
                 :total-size size))

(defmethod make-container ((class (eql 'ring-buffer)) &rest args)
  (let ((total-size (getf args :total-size 1))
        (last-in-first-out (getf args :last-in-first-out nil)))
    (remf args :total-size)
    (remf args :last-in-first-out)
    (make-ring-buffer total-size last-in-first-out)))


;;?? the (first indexes) is odd...
(defmethod item-at ((container ring-buffer) &rest indexes)
  "Return the ring-buffer element corresponding to the given index.
The indexing is from oldest to newest with a `ring-buffer' and from newest to
oldest with a `ring-buffer-reverse'.
Warning: Only the first element of INDEXES is used."
  (declare (dynamic-extent indexes))
  (let ((index (%index container (first indexes))))
    (svref (contents container)
           (mod index (total-size container)))))

(defmethod item-at! ((container ring-buffer) value &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((index (%index container (first indexes))))
    (setf (svref (contents container)
                 (mod index (total-size container)))
          value)))

(defmethod %index ((container ring-buffer-reverse) index)
  "Return index converted to internal LIFO index, where items are ordered from
newest to oldest."
  (if (empty-p container)
      (error "Tried to index empty ring-buffer")
      (1- (+ (buffer-start container)
             (- (size container) (mod index (size container)))))))

(defmethod %index ((container ring-buffer) index)
  "Return index converted to internal FIFO index, where items are ordered from
oldest to newest."
  (if (empty-p container)
      (error "Tried to index empty ring-buffer")
      (+ (buffer-start container) (mod index (size container)))))

(defmethod container->list ((container ring-buffer))
  "Return list of items.
Items are ordered from oldest to newest if `ring-buffer', or from newest to
oldest if `ring-buffer-reverse'."
  (loop for index from 0 below (size container)
        for item = (item-at container index)
        when item collect item))

(defmethod increment-end ((container ring-buffer))
  (with-slots (buffer-end buffer-start) container
    (when (and (>= buffer-end (total-size container))
               (= (mod buffer-end (total-size container)) buffer-start))
      (incf buffer-start))
    (incf buffer-end)))

(defmethod delete-item-at ((container ring-buffer) &rest indexes)
  "Delete item using FIFO or LIFO index for ring-buffer.
Warning: Only the first element of INDEXES is used."
  (let* ((index (first indexes))
         (first-half? (< index (/ (size container) 2)))
         (end (if first-half?
                  0
                  (size container)))
         (next-index (if first-half? #'1- #'1+))
         (shift-value (lambda (i) (item-at! container
                                            (item-at container
                                                     (funcall next-index i))
                                            i))))
    (if first-half?
        (loop :for i :from index :above end
              :do (funcall shift-value i))
        (loop :for i :from index :below end
              :do (funcall shift-value i)))
    (if first-half?
        (delete-first container)
        (delete-last container))))

(defmethod delete-item ((container ring-buffer) item)
  (let ((pos (element-position container item)))
    (when pos (delete-item-at container pos))))

(defmethod next-item ((container ring-buffer))
  (increment-end container)
  (current-item container))


(defmethod current-item ((container ring-buffer))
  "Last item in the ordered container.
For `ring-buffer', it's the newest item.
For `ring-buffer-reverse', it's the oldest item."
  (item-at container (1- (size container))))


(defmethod insert-item ((container ring-buffer) item)
  (prog1
      (setf (svref (contents container)
                   (mod (buffer-end container) (total-size container)))
            item)
    (increment-end container)))


(defmethod delete-first ((container ring-buffer))
  (unless (empty-p container)
    (prog1
        (first-item container)
      (incf (slot-value container 'buffer-start)))))

(defmethod delete-last ((container ring-buffer))
  (unless (empty-p container)
    (prog1
        (last-item container)
      (decf (slot-value container 'buffer-end)))))

(defmethod delete-first ((container ring-buffer-reverse))
  (unless (empty-p container)
    (prog1
        (first-item container)
      (decf (slot-value container 'buffer-end)))))

(defmethod delete-last ((container ring-buffer-reverse))
  (unless (empty-p container)
    (prog1
        (last-item container)
      (incf (slot-value container 'buffer-start)))))


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
  "Equivalent to `first-item'."
  (item-at container 0))

(defmethod (setf first-element) (value (container ring-buffer))
  (setf (item-at container 0) value))

(defmethod last-element ((container ring-buffer))
  "Equivalent to `last-item'."
  (item-at container (1- (size container))))

(defmethod (setf last-element) (value (container ring-buffer))
  (setf (item-at container (1- (size container))) value))

(defmethod iterate-nodes ((container ring-buffer) fn)
  (loop for index from 0 below (size container)
        do (funcall fn (item-at container index))))

#+No
;; screws with the buffer pointers
(defmethod iterate-nodes ((container ring-buffer) fn)
  (loop repeat (total-size container)
        with item = (current-item container) do
          (funcall fn item)
          (setf item (next-item container))))
