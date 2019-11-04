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
  ((contents :reader contents
             :initarg :contents)
   (buffer-start :reader buffer-start
                 :initform 0)
   (buffer-end :reader buffer-end
               :initform 0)
   (total-size :reader total-size
               :initarg :total-size)))

(defclass* ring-buffer-reverse (ring-buffer) ())

(defun make-ring-buffer (size last-in-first-out)
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
  (declare (dynamic-extent indexes))
  (svref (contents container)
         (mod (first indexes) (total-size container))))

(defmethod item-at ((container ring-buffer-reverse) &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((indexes (mapcar #'(lambda (index) (lifo-index container index))
                         indexes)))
    (svref (contents container)
           (mod (first indexes) (total-size container)))))

(defmethod item-at! ((container ring-buffer) value &rest indexes)
  (declare (dynamic-extent indexes))
  (setf (svref (contents container)
               (mod (first indexes) (total-size container)))
        value))

(defmethod item-at! ((container ring-buffer-reverse) value &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((indexes (mapcar #'(lambda (index) (lifo-index container index))
                         indexes)))
    (setf (svref (contents container)
                 (mod (first indexes) (total-size container)))
          value)))

(defmethod lifo-index ((container ring-buffer-reverse) index)
  "Return index converted to internal LIFO index, where items are ordered from newest to oldest."
  (mod (1- (+ (buffer-start container)
              (- (buffer-end container) (buffer-start container) index)))
       (total-size container)))

(defmethod recent-list ((container ring-buffer-reverse))
  "Return list of items ordered by most recent."
  (loop for index from 0 below (- (buffer-end container) (buffer-start container))
        for item = (item-at container index)
        when item collect item))

(defmethod increment-end ((container ring-buffer))
  (with-slots (buffer-end buffer-start) container
    (when (and (>= buffer-end (total-size container))
               (= (mod buffer-end (total-size container)) buffer-start))
      (incf buffer-start))
    (incf buffer-end)))

(defmethod delete-item-at ((container ring-buffer-reverse) &rest indexes)
  "Delete item by LIFO-INDEX ([0, size-1] where 0 is most recent)."
  (let ((index (car indexes)))
    (with-slots (buffer-end buffer-start) container
      (if (= index 0)
          (setf (aref (contents container) (lifo-index container 0)) nil)
          (if (> index (/ (total-size container) 2))
              (progn
                (loop :for i :from index :downto 1 do
                  (setf (aref (contents container)
                              (lifo-index container i))
                        (aref (contents container)
                              (lifo-index container (1- i)))))
                (setf (aref (contents container) (lifo-index container 0)) nil)
                (decf buffer-end))
              (progn
                (loop :for i :from index :to (1- (total-size container)) do
                  (setf (aref (contents container)
                              (lifo-index container i))
                        (aref (contents container)
                              (lifo-index container (1+ i)))))
                (setf (aref (contents container) (lifo-index container (1- (total-size container)))) nil)
                (incf buffer-start)))))))


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
