(in-package #:cl-containers)

#|
IF we were going to re-write, would we write collect / iterate in terms
of iterators or vis verso?!

reset

size
  hard to do generally

class hierarchy isn't quite right for generators yet

this isn't quite the right command set

forward, backward, both

costs

using for multi-container transforms, etc.

??does it start on the first item or off the end of the container...

I'm confused about how to integrate starting off the end with filters, etc.
  if I call move-forward-p, then I have to check that the next-element passes.
  I could save the current element (side effects?) and move to the next with move-forward-p
  and then really move when we call move... Sounds tricky and  ke a lot of 
  bookkeeping.

  Suppose I start at the first element (if any). Then I need a way to know that
  I'm on an element.

arrays

other containers
  ordered, etc.

random-number-generators

|#

#|
element returns current-element and allows for side-effects
  current-element only does the access (used, for example, 
  in unique-value-iterator-mixin
|#


(defconstant +iterator-before-beginning+ :pre)

(defconstant +iterator-after-end+ :post)

(metatilities:defcondition basic-iterator-condition ()
  ((iterator nil ir))
  (:export-p t)
  (:export-slots-p t))

(metatilities:defcondition no-current-element-error
    (basic-iterator-condition error)
  ()
  (:export-p t)
  (:export-slots-p t))

(defclass* abstract-generator ()
  ((iterator-position +iterator-before-beginning+ r)))

(defmethod finish ((iterator abstract-generator))
  (values))

(defclass* basic-iterator (abstract-generator)
  ((initial-container nil ir :initarg :container)
   (iterating-container nil r)))

(defmethod initialize-instance
    :after ((object basic-iterator) &key &allow-other-keys)
  (setup-initial-container object))

(defmethod setup-initial-container ((object basic-iterator))
  (setf (slot-value object 'iterating-container)
        (if (typep (initial-container object) 'uses-contents-mixin)
          (contents (initial-container object))
          (initial-container object))))

(defmethod print-iterator ((iterator abstract-generator) stream)
  (declare (ignore stream))
  (values))

(defmethod reset :before ((iterator abstract-generator))
  (setf (slot-value iterator 'iterator-position)
	+iterator-before-beginning+))

(defmethod move-p ((iterator abstract-generator) direction)
  (declare (ignore direction))
  (values nil))

(defmethod print-object ((object abstract-generator) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (print-iterator object stream)))

#+Ignore
(defmethod size ((iterator basic-iterator))
  ;; a decent general method
  (size (container iterator)))

(defmethod print-iterator ((iterator basic-iterator) stream)
  (format stream "~D" (current-element-p iterator)))

(defclass* forward-iterator (basic-iterator iteratable-container-mixin)
  ())

(defmethod iterate-container ((iterator forward-iterator) fn)
  (iterate-forward iterator fn))

(defmethod iterate-nodes ((iterator forward-iterator) fn)
  (iterate-forward iterator fn))

(defmethod empty-p ((object forward-iterator))
  (not (move-forward-p object)))

(defmethod element ((iterator abstract-generator))
  (current-element iterator))

(defmethod next-element ((iterator abstract-generator))
  (move-forward iterator)
  (current-element iterator))

(defmethod current-element-p ((iterator basic-iterator))
  (null (iterator-position iterator)))

(defmethod element :around ((iterator abstract-generator))
  (unless (current-element-p iterator)
    (error 'no-current-element-error :iterator iterator))
  (call-next-method))

(defmethod move :around ((iterator basic-iterator) direction)
  (cond ((iterator-position iterator)
         (setf (slot-value iterator 'iterator-position) nil))
        (t
         (call-next-method)))
  (unless (move-p iterator direction)
    (setf (slot-value iterator 'iterator-position)
          +iterator-after-end+)))

(defun move-forward-p (iterator)
  (move-p iterator :forward))

(defmethod move-forward ((iterator forward-iterator))
  (move iterator :forward))

(defmethod iterate-forward ((iterator basic-iterator) function)
  (loop while (move-forward-p iterator) do
        (when (current-element-p iterator)
          (funcall function (element iterator)))
        (move-forward iterator)))


;;; transforming-iterator-mixin

(defclass* transforming-iterator-mixin ()
  ((transform nil ir)
   (transformed-element nil r)
   (compute-element? t r)))

(defmethod current-element :around ((iterator transforming-iterator-mixin))
  (cond ((compute-element? iterator)
         (setf (slot-value iterator 'compute-element?) nil
               (slot-value iterator 'transformed-element)
               (funcall (transform iterator) (call-next-method))))
        (t
         (transformed-element iterator))))

(defmethod move :after ((iterator transforming-iterator-mixin) direction)
  (declare (ignorable direction))
  (setf (slot-value iterator 'compute-element?) t))


;;; filtered-iterator-mixin

(defclass* basic-filtered-iterator-mixin ()
  ())

(defmethod move :after ((iterator basic-filtered-iterator-mixin) 
                        (direction (eql :forward)))
  (move-forward-to-next-element iterator))

(defclass* filtered-iterator-mixin (basic-filtered-iterator-mixin)
  ((filter nil ir)))


(defmethod element-passes-p and ((iterator filtered-iterator-mixin))
  (funcall (filter iterator) (current-element iterator)))

(defmethod initialize-instance :around ((object filtered-iterator-mixin) &key)
  (prog1
    (call-next-method)
    (move-forward-to-next-element object)))

(defmethod move-forward-to-next-element ((iterator basic-iterator))
  (loop while (and (move-p iterator :forward)
                   (or (not (current-element-p iterator))
                       (not (element-passes-p iterator)))) do
        (move iterator :forward)))
        

;;; unique-value-iterator-mixin

(defclass* unique-value-iterator-mixin (basic-filtered-iterator-mixin 
                                          test-container-mixin)
  ((visited nil ir)))

(defmethod initialize-instance
    :after ((object unique-value-iterator-mixin) &key)
  (setf (slot-value object 'visited)
        (make-container 'simple-associative-container
                        :test (test object))))

(defmethod element-passes-p and ((iterator unique-value-iterator-mixin))
  (not (item-at (visited iterator) (current-element iterator))))

(defmethod element :around ((iterator unique-value-iterator-mixin))
  (let ((element (call-next-method)))
    (setf (item-at-1 (visited iterator) element) t)
    element))

#|
(defclass* test-iteration (filtered-iterator-mixin
                             unique-value-iterator-mixin
                             list-iterator)
  ())

(let ((l '(2 3 4 5 5 5 5 6 7)))
  (iterate-forward 
   (make-instance 'test-iteration
     :pointer l
     :filter #'oddp
     )
   #'print)
  (values))
|#

;;; circular iterators -- they just don't stop

(defclass* circular-iterator-mixin ()
  ())

(defmethod move-p ((iterator circular-iterator-mixin) 
		   (direction (eql :forward)))
  (unless (call-next-method)
    (reset iterator)
    ;;?? perhaps an ugly hack?!
    (move-forward iterator))
  (values t))

#+Test
(let ((i (make-iterator '(1 2 3) :circular t)))
  (loop repeat 10 do
        (move-forward i)
        (print (current-element i))))

#+Test
(let ((i (make-iterator '(1 2 3) :circular t)))
  (loop repeat 10 do
        (print (next-element i))))

;;; list-iterator

(defclass* list-iterator (forward-iterator)
  ())

(defmethod reset ((iterator list-iterator))
  (setup-initial-container iterator)
  iterator)

(defmethod move ((iterator list-iterator) (direction (eql :forward)))
  (setf (slot-value iterator 'iterating-container) 
        (rest (iterating-container iterator)))
  (values))

(defmethod current-element ((iterator list-iterator))
  (first (iterating-container iterator)))

(defmethod move-p ((iterator list-iterator) (direction (eql :forward)))
  (not (null (iterating-container iterator))))


;;; arrays

(defclass* array-iterator (forward-iterator)
  ((index 0 ir)))

(defmethod reset ((iterator array-iterator))
  (setf (slot-value iterator 'index) 0)
  iterator)

(defmethod move ((iterator array-iterator) (direction (eql :forward)))
  (incf (slot-value iterator 'index))
  (values))

(defmethod current-element-p ((iterator array-iterator))
  (and (call-next-method)
       (< (index iterator) (size (iterating-container iterator)))))

(defmethod current-element ((iterator array-iterator ))
  (row-major-aref (iterating-container iterator) (index iterator)))

(defmethod move-p ((iterator array-iterator ) (direction (eql :forward)))
  (< (index iterator) (size (iterating-container iterator))))

;;; hash-table-iterator

(defclass* hash-table-iterator (list-iterator)
  ())

(defmethod initialize-instance :after ((object hash-table-iterator) &key)
  (reset object))

(defmethod reset ((iterator hash-table-iterator))
  (setf (slot-value iterator 'iterating-container) 
        (collect-elements (initial-container iterator)))
  iterator)


;;; make-iterator

(defmethod make-iterator
    (iteratee &rest args &key (iterator-class nil) &allow-other-keys)
  (apply #'make-instance 
	 (or 
	  (metatilities:apply-if-exists
	   'determine-iterator-class 'cl-containers
	   iteratee iterator-class args)
	  iterator-class)
         :container iteratee
         args))

(defmethod make-iterator
    ((iteratee basic-iterator) &rest args &key &allow-other-keys)
  (when args 
    (warn "make-iterator not using args with ~a, ~a" 
	  iteratee args))
  iteratee)

;;; some generators

(defun make-generator (&rest args &key (generator-class nil) &allow-other-keys)
  (apply #'make-instance 
	 (or
	  (apply-if-exists 'determine-generator-class 'cl-containers
			   generator-class args)
	  generator-class) args))


;;; sequences

(defclass* basic-generator (forward-iterator)
  ())

(defclass* arithmetic-sequence-generator (basic-generator)
  ((start 0 ir)
   (by 1 ir)
   (element nil r)))

(defmethod initialize-instance 
    :after ((object arithmetic-sequence-generator) &key)
  (setf (slot-value object 'element) (start object)))

(defmethod move ((iterator arithmetic-sequence-generator)
		 (direction (eql :forward)))
  (incf (slot-value iterator 'element) (by iterator)))

(defmethod move-p ((iterator arithmetic-sequence-generator)
		   (direction (eql :forward)))
  (values t))

(defmethod current-element ((iterator arithmetic-sequence-generator))
  (slot-value iterator 'element))

(defclass* finite-arithmetic-sequence-generator 
    (arithmetic-sequence-generator)
  ((end 0 ir)))

(defmethod move-p ((iterator finite-arithmetic-sequence-generator)
		   (direction (eql :forward)))
  (<= (current-element iterator) (end iterator)))

#|
(collect-elements
 (make-generator :end 10 :start 5 :generator-class 'finite-arithmetic-sequence-generator))

(collect-elements
 (make-generator :end 10 :start 5))

(collect-elements
 (make-iterator '(1 2 3) :iterator-class 'list-iterator))

(collect-items
 (make-iterator '(1 2 3)))

(move-forward-p ccl:!)
(move-forward ccl:!)
(current-element-p ccl:!)
(current-element ccl:!)
(compute-applicable-methods #'move-forward-p (list ccl:!))

(collect-elements
 (make-generator :end 10 :start 5 :transform #'u:square))

(subtypep 'finite-arithmetic-sequence-generator 'abstract-generator) 

(remove-redundant-classes
 '(finite-arithmetic-sequence-generator
   arithmetic-sequence-generator transforming-iterator-mixin
   abstract-generator))
|#

;;; map-containers

;;?? very consy
(defun map-containers (fn &rest containers)
  (let ((iterators (mapcar #'make-iterator containers)))
    (loop while (every #'move-forward-p iterators) do
          (apply fn (mapcar #'current-element iterators))
          (mapc #'move-forward iterators))))

(defun collect-containers (fn &rest containers)
  (let ((result nil))
    (apply #'map-containers (lambda (&rest args)
                              (push (apply fn args) result))
           containers)
    (nreverse result)))

(defmacro with-iterator ((var source &rest args) &body body)
  `(let (,var)
     (unwind-protect
       (progn
         (setf ,var (make-iterator ,source ,@args))
         ,@body)
       (when ,var (finish ,var)))))

