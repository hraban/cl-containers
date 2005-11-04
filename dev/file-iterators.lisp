(in-package containers)

#|
file lines
file forms
string as characters
string as words
string as, e.g., paragraphs

stemming
|#

(defclass* basic-stream-iterator (forward-iterator)
  ((stream nil ir)
   (close? nil r)))

;;; ---------------------------------------------------------------------------
   
(defmethod initialize-instance :after ((object basic-stream-iterator) &key container)
  (setf (values (slot-value object 'stream)
                (slot-value object 'close?))
        (open-file-for-iterator object container))
  
  (advance object)
  
  ;; if garbage collected close the stream
  (ccl:terminate-when-unreachable object))

;;; ---------------------------------------------------------------------------

(defmethod terminate ((iterator basic-stream-iterator))
  ;;??
  (format t "GC: Maybe closing stream" iterator)
  (when (and (close? iterator)
             (streamp (stream iterator))
             (open-stream-p (stream iterator)))
    (close (stream iterator))))

;;; ---------------------------------------------------------------------------

(defmethod open-file-for-iterator ((object basic-stream-iterator) (filename string))
  (values (open filename :if-does-not-exist :error
                :direction :input)
          t))

;;; ---------------------------------------------------------------------------

(defmethod open-file-for-iterator ((object basic-stream-iterator) (filename pathname))
  (open-file-for-iterator object (namestring filename)))

;;; ---------------------------------------------------------------------------

(defmethod open-file-for-iterator ((object basic-stream-iterator) (filename stream))
  (values filename nil))


;;; ---------------------------------------------------------------------------
;;; file-iterator
;;;
;;;?? assume that someone else is handling buffering for now...
;;; ---------------------------------------------------------------------------

(defclass* file-iterator (basic-stream-iterator)
  ((current-char nil r)))

;;; ---------------------------------------------------------------------------

(defmethod base-class-for-iteratee ((container pathname))
  'file-iterator)

;;; ---------------------------------------------------------------------------

(defmethod move ((iterator file-iterator) (direction (eql :forward)))
  (advance iterator))

;;; ---------------------------------------------------------------------------

(defmethod advance ((iterator file-iterator))
  (setf (slot-value iterator 'current-char) 
        (read-char (stream iterator) nil :eof)))

;;; ---------------------------------------------------------------------------

(defmethod current-element ((iterator file-iterator))
  (current-char iterator))

;;; ---------------------------------------------------------------------------

(defmethod current-element-p ((iterator file-iterator))
  (and (call-next-method)
       (not (eq (current-char iterator) :eof))))

;;; ---------------------------------------------------------------------------

(defmethod move-p ((iterator file-iterator) (direction (eql :forward)))
  (not (eq (current-char iterator) :eof)))


;;; ---------------------------------------------------------------------------
;;; file-line-iterator
;;; ---------------------------------------------------------------------------

(defclass* file-line-iterator (basic-stream-iterator)
  ((current-line nil r)))

;;; ---------------------------------------------------------------------------

(defmethod move ((iterator file-line-iterator) (direction (eql :forward)))
  (advance iterator))

;;; ---------------------------------------------------------------------------

(defmethod advance ((iterator file-line-iterator))
  (setf (slot-value iterator 'current-line) 
        (read-line (stream iterator) nil :eof)))

;;; ---------------------------------------------------------------------------

(defmethod current-element ((iterator file-line-iterator))
  (current-line iterator))

;;; ---------------------------------------------------------------------------

(defmethod current-element-p ((iterator file-line-iterator))
  (and (call-next-method)
       (not (eq (current-line iterator) :eof))))

;;; ---------------------------------------------------------------------------

(defmethod move-p ((iterator file-line-iterator) (direction (eql :forward)))
  (not (eq (current-line iterator) :eof)))

;;; ---------------------------------------------------------------------------

(defmethod class-for-contents-as ((contents string) (as (eql :file-lines)))
  'file-line-iterator)


;;; ---------------------------------------------------------------------------
;;; word-iterator
;;; ---------------------------------------------------------------------------

(defclass* word-iterator (forward-iterator)
  ((cache (make-array 20 :element-type 'character :fill-pointer 0 :adjustable t) r)
   (current-word nil r)
   (internal-iterator nil r)))

;;; ---------------------------------------------------------------------------

(defmethod initialize-instance :after ((object word-iterator) &key container)
  (setf (slot-value object 'internal-iterator) 
        (make-iterator container))
  (when (move-forward-p (internal-iterator object))
    (move-forward (internal-iterator object)))
  (advance object))

;;; ---------------------------------------------------------------------------

(defmethod delimiter-p ((iterator word-iterator) (thing character))
  (metatilities:whitespacep thing))

;;; ---------------------------------------------------------------------------

(defmethod move ((iterator word-iterator) (direction (eql :forward)))
  (advance iterator))

;;; ---------------------------------------------------------------------------

(defmethod advance ((iterator word-iterator))
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

(defmethod current-element ((iterator word-iterator))
  (current-word iterator))

;;; ---------------------------------------------------------------------------

(defmethod current-element-p ((iterator word-iterator))
  (and (call-next-method)
       (plusp (fill-pointer (cache iterator)))))

;;; ---------------------------------------------------------------------------

(defmethod move-p ((iterator word-iterator) (direction (eql :forward)))
  (or (move-p (internal-iterator iterator) direction)
      (plusp (size (cache iterator)))))

;;; ---------------------------------------------------------------------------

(defmethod class-for-contents-as ((contents string) (as (eql :words)))
  'word-iterator)




