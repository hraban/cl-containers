(in-package #:containers)

#|
- file lines
file forms
string as characters
string as words
string as, e.g., paragraphs

stemming
|#

(defclass* basic-stream-iterator (forward-iterator)
  ((stream nil i :reader iterator-stream)
   (close? nil r)))

(defmethod initialize-instance :after
    ((object basic-stream-iterator) &key container)
  (setf (values (slot-value object 'stream)
                (slot-value object 'close?))
        (open-file-for-iterator object container))
  
  (advance object)
  ;; if garbage collected close the stream
  (funcall-if-exists 'care-when-finalized 'mopu object))

(defmethod finish ((iterator basic-stream-iterator))
  (when (and (close? iterator)
             (streamp (iterator-stream iterator))
             (open-stream-p (iterator-stream iterator)))
    (close (iterator-stream iterator))
    (setf (slot-value iterator 'close?) nil)))

(defmethod open-file-for-iterator
    ((object basic-stream-iterator) (filename string))
  (values (open filename :if-does-not-exist :error
                :direction :input)
          t))

(defmethod open-file-for-iterator
    ((object basic-stream-iterator) (filename pathname))
  (open-file-for-iterator object (namestring filename)))

(defmethod open-file-for-iterator
    ((object basic-stream-iterator) (filename stream))
  (values filename nil))


;;; file-iterator
;;;
;;;?? assume that someone else is handling buffering for now...

(defclass* file-iterator (basic-stream-iterator)
  ((current-char nil r)))

(defmethod base-class-for-iteratee ((container pathname))
  'file-iterator)

(defmethod move ((iterator file-iterator) (direction (eql :forward)))
  (advance iterator))

(defmethod advance ((iterator file-iterator))
  (setf (slot-value iterator 'current-char) 
        (read-char (iterator-stream iterator) nil :eof)))

(defmethod current-element ((iterator file-iterator))
  (current-char iterator))

(defmethod current-element-p ((iterator file-iterator))
  (and (call-next-method)
       (not (eq (current-char iterator) :eof))))

(defmethod move-p ((iterator file-iterator) (direction (eql :forward)))
  (not (eq (current-char iterator) :eof)))


;;; file-line-iterator

(defclass* file-line-iterator (basic-stream-iterator)
  ((current-line nil r)))

(defmethod move ((iterator file-line-iterator) (direction (eql :forward)))
  (advance iterator))

(defmethod advance ((iterator file-line-iterator))
  (setf (slot-value iterator 'current-line) 
        (read-line (iterator-stream iterator) nil :eof)))

(defmethod current-element ((iterator file-line-iterator))
  (current-line iterator))

(defmethod current-element-p ((iterator file-line-iterator))
  (and (call-next-method)
       (not (eq (current-line iterator) :eof))))

(defmethod move-p ((iterator file-line-iterator) (direction (eql :forward)))
  (not (eq (current-line iterator) :eof)))

(defmethod class-for-contents-as ((contents pathname) (as (eql :lines)))
  'file-line-iterator)

;;; file-form-iterator

(defclass* file-form-iterator (basic-stream-iterator)
  ((current-form nil r)))


(defmethod move ((iterator file-form-iterator) (direction (eql :forward)))
  (advance iterator))


(defmethod advance ((iterator file-form-iterator))
  (setf (slot-value iterator 'current-form) 
        (read (iterator-stream iterator) nil :eof)))


(defmethod current-element ((iterator file-form-iterator))
  (current-form iterator))


(defmethod current-element-p ((iterator file-form-iterator))
  (and (call-next-method)
       (not (eq (current-form iterator) :eof))))


(defmethod move-p ((iterator file-form-iterator) (direction (eql :forward)))
  (not (eq (current-form iterator) :eof)))


(defmethod class-for-contents-as ((contents pathname) (as (eql :forms)))
  'file-form-iterator)


;;; delimited-iterator

(defclass* delimited-iterator (forward-iterator)
  ((cache (make-array 20 :element-type 'character
		      :fill-pointer 0 :adjustable t) r)
   (current-chunk nil r)
   (internal-iterator nil r)
   (element-characterizer 'metatilities:whitespacep ia)
   (skip-empty-chunks? t ia)
   (starting-element nil a)))


(defclass* internal-iterator-mixin ()
  ((iterator nil ir)))


(defmethod initialize-instance :after ((object delimited-iterator) &key container
                                       &allow-other-keys)
  (setf (slot-value object 'internal-iterator) 
        (make-internal-iterator object container))
  (when (move-forward-p (internal-iterator object))
    (move-forward (internal-iterator object)))
  (advance object))


(defmethod make-internal-iterator ((object delimited-iterator) container)
  (make-iterator container 
                 :iterator-class 'internal-iterator-mixin
                 :iterator object))


(defgeneric characterize-element (iterator element)
  (:documentation "Examines element in the context of iterator and returns a value describing how to treat it. This can be one of:

* nil or :appended - append element to the current chunk
* t or :delimiter  - complete the current chunk and start a new one \(ignore element\)
* :ignored         - act as if this element was never seen
* :start-new       - complete the current chunk and start a new one with this element
"))

(defmethod characterize-element ((iterator delimited-iterator) (thing t))
  (funcall (element-characterizer iterator) thing))


(defmethod move ((iterator delimited-iterator) (direction (eql :forward)))
  (advance iterator))


(defmethod move-internal ((iterator delimited-iterator) (direction (eql :forward)))
  (move-forward (internal-iterator iterator)))


(defmethod advance ((iterator delimited-iterator))
  (let ((internal (internal-iterator iterator)))
    (setf (fill-pointer (cache iterator)) 0)
    (when (starting-element iterator)
      (vector-push-extend (starting-element iterator) (cache iterator))
      (setf (starting-element iterator) nil))
    (loop while (move-forward-p internal) do
          (let ((element-is (characterize-element iterator (current-element internal))))
            ;(format t "~%~A ~A" (current-element internal) element-is)
            (case element-is
              ((nil :appended) 
               (vector-push-extend (current-element internal) (cache iterator)))
              ((t :delimiter)
               (if (skip-empty-chunks? iterator)
                 (loop while (and (move-forward-p internal)
                                  (member (characterize-element
                                           iterator (current-element internal))
                                          '(:ignored :delimiter t))) do
                       ;(format t "~%  '~A'" (current-element internal))
                       (move-internal iterator :forward))
                 
                 (move-internal iterator :forward))
               ;; leave loop
               (return))
              (:ignored nil)
              (:start-new 
               (setf (starting-element iterator) (current-element internal))
               (move-internal iterator :forward)
               ;; leave loop
               (return))
              (t 
               (warn "Don't know how to ~S ~S" element-is (current-element internal)))))
      
          (move-forward internal))
    (setf (slot-value iterator 'current-chunk)
          (combine-elements iterator))))


(defmethod combine-elements ((iterator delimited-iterator)) 
  (format nil "~A" (coerce (cache iterator) 'string)))

#+Experimental
;;?? trying to guarentee single calls to characterize-element (this doesn't work)
(defmethod advance ((iterator delimited-iterator))
  (let ((internal (internal-iterator iterator))
        (first? t))
    (setf (fill-pointer (cache iterator)) 0) 
    (loop while (move-forward-p internal) do
          (when (characterize-element iterator (current-element internal))
            (if (skip-empty-chunks? iterator)
              (loop while (or first?
                              (and (move-forward-p internal)
                                   (characterize-element iterator (current-element internal)))) do
                    (setf first? nil)
                    (move-forward internal))
              
              (move-forward internal))
            (return))
          (vector-push-extend (current-element internal) (cache iterator))
          (move-forward internal))
    (setf (slot-value iterator 'current-chunk)
          (coerce (cache iterator) 'string))))


(defmethod current-element ((iterator delimited-iterator))
  (current-chunk iterator))


(defmethod current-element-p ((iterator delimited-iterator))
  (and (call-next-method)
       (or (not (skip-empty-chunks? iterator))
           (plusp (fill-pointer (cache iterator))))))

(defmethod move-p ((iterator delimited-iterator) (direction (eql :forward)))
  (or (move-p (internal-iterator iterator) direction)
      (plusp (size (cache iterator)))))

(defclass* word-iterator (delimited-iterator)
  ()
  (:default-initargs
    :element-characterizer 'metatilities:whitespacep))

(defclass* line-iterator (delimited-iterator)
  ()
  (:default-initargs
    :element-characterizer (lambda (ch) (or (eq ch #\linefeed)
                                            (eq ch #\newline)
                                            (eq ch #\return)))))

(defmethod class-for-contents-as ((contents t) (as (eql :lines)))
  'line-iterator)

(defmethod class-for-contents-as ((contents t) (as (eql :words)))
  'word-iterator)



#|
(collect-elements (make-iterator "this is
paragraph number one.

this is paragraph number two.




and this
is
paragraph number
three." :treat-contents-as :lines))

(collect-elements (make-iterator #P"user-home:qt.lisp" :treat-contents-as :lines))
|#



