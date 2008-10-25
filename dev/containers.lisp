
(in-package #:containers)


(defparameter +empty-initial-element+ (cons :nothing nil))


(defclass* abstract-container ()
  ()
  (:documentation "Inherited by all container classes, this is a good place to
put those pesky superclasses you need everyone to inherit.")
  (:export-p t))


(defclass* concrete-container ()
  ()
  (:documentation "Inherited by all container classes that can/should 
be instantiated using make-container."))


(defclass* container-node-mixin ()
  ((element nil ia))                   ;?? :unbound
  (:export-slots element)
  (:export-p t))


(defclass* parent-node-mixin (container-node-mixin)
  ((parent nil ia))
  (:documentation "A mixin for nodes with parent pointers"))


(defclass* two-child-node (parent-node-mixin)
  ((right-child nil a)
   (left-child  nil a)
   (parent nil ia)))


;;; Generic container class and various generic operations

;;; Various useful (?) container mixins

;;; keyed containers
;;; supports: key
;;;

(defclass* keyed-container-mixin (abstract-container)
  ((key :initform 'identity ; 20020126, LST: changed to a bare symbol, since you can dump that.
        :initarg :key
        :reader key)))


;;; Typed containers
;;; supports: element-type
;;;

(defclass* typed-container-mixin (abstract-container)
  ((element-type :initform nil
                 :initarg :element-type
                 :reader element-type)))


;;; Bounded containers (containers of fixed size)
;;; supports: total-size
;;;

(defclass* bounded-container-mixin (abstract-container)
  ())


;;; Indexed containers (containers you can point into)
;;; supports: item-at, (setf item-at)
;;;

(defclass* indexed-container-mixin (abstract-container)
  ())


(defmethod (setf item-at) (value container &rest indexes)
  (apply #'item-at! container value indexes))


;;; initial-element-mixin
;;; supports: initial-element
;;;

(defclass* initial-element-mixin (abstract-container)
  ((initial-element :unbound ia)
   (initial-element-fn nil ir
                       :initarg :initial-element-function
                       :reader initial-element-function)
   (has-initial-element-p nil r)))


(defmethod initialize-instance :after ((object initial-element-mixin) &key)
  (assert (or (null (initial-element-fn object))
              (functionp (initial-element-fn object))
              (and (symbolp (initial-element-fn object))
                   (symbol-function (initial-element-fn object))))
          nil
          "Initial-element-fn must be nil or a function.")
  
  (when (and (initial-element-fn object)
             (symbolp (initial-element-fn object))
             (symbol-function (initial-element-fn object)))
    (setf (slot-value object 'initial-element-fn)
          (symbol-function (initial-element-fn object))))
  
  (when (or (initial-element-fn object)
            (slot-boundp object 'initial-element))
    (setf (slot-value object 'has-initial-element-p)
          t
          (slot-value object 'initial-element-fn)
          (cond ((initial-element-fn object)
                 (initial-element-fn object))
                ((slot-boundp object 'initial-element)
                 (lambda ()
                   (initial-element object)))))))


(defun return-empty-initial-element ()
  +empty-initial-element+)


;;?? Gary King 2004-05-01: this could be optimized into a single call at
;; creation time -- i.e., do the tests then once, not now many times.
(defmethod make-initial-element ((container initial-element-mixin))
  (funcall (initial-element-fn container)))


;;; initial-contents-mixin
;;; supports: initial-contents
;;;

(defclass* basic-initial-contents-mixin (abstract-container)
  ())


(defclass* initial-contents-mixin (basic-initial-contents-mixin)
  ()
  (:export-p t))

(defclass* initial-contents-key-value-mixin (basic-initial-contents-mixin)
  ())


(defmethod initialize-instance :after ((object basic-initial-contents-mixin) 
                                       &key initial-contents)
  (when (insert-initial-contents-p object)
    (add-initial-contents object initial-contents)))


;;; findable-containers, searchable-containers
;;; supports: test, find-item or search-for-item and search-for-match
;;;
;;; A searchable container is any one that can be searched even if it's not
;;; efficient. A findable-container incorporates a pre-specified search into
;;; its construction so that things can be found fast.

(defclass* test-container-mixin ()
  ((test :initform 'eq
         :initarg :test
         :reader test)))

;;; container-uses-nodes-mixin
;;;
;;; A container that puts things into nodes (like a binary-search-tree or a heap)
;;;

(defclass* container-uses-nodes-mixin (abstract-container)
  ()
  (:export-p t))


(defclass* findable-container-mixin (test-container-mixin 
                                     keyed-container-mixin
                                     abstract-container)
  ())


(defclass* searchable-container-mixin (test-container-mixin abstract-container)
  ())


;;; Iterate-able containers (containers you can run over...)
;;; supports: iterate-nodes [subclasses should override]
;;;  NOTES: If you subclass from this and implement iterate-nodes,
;;;   then you get print-container, collect-items,
;;;   search-for-item, and delete-item-if for free.

(defclass* iteratable-container-mixin (searchable-container-mixin)
  ())


(defclass* key-value-iteratable-container-mixin (iteratable-container-mixin)
  ())


;;?? deprecate
;;?? backward compatibility
(defmethod iterate-container (object fn)
  (funcall #'iterate-nodes object fn))

(defmethod collect-items (object &rest args &key filter transform)
  (declare (dynamic-extent args)
           (ignore filter transform))
  (apply #'collect-nodes object args))


;;; i-know-my-node-mixin

;; This would be inherited by elements inserted into the container...
;; It is a hack for optimization and speed (if it had a better name, 
;; you would probably call it a trick...)
(defclass* i-know-my-node-mixin ()
  ((my-node nil iar))
  :export-slots
  (:export-p t))

(defmethod make-node-for-container :around ((container t) (item i-know-my-node-mixin) 
                                            &key)
  (let ((node (call-next-method)))
    (setf (my-node item) node)
    (values node)))


;;; unordered-container-mixin
;;; the items are not in any order (e.g., a bag)

(defclass* non-associative-container-mixin (abstract-container)
  ()
  (:export-p t)
  (:documentation "A non associative container should implement at least empty-p,
empty, insert-item and delete-item."))


(defclass* unordered-container-mixin (non-associative-container-mixin)
  ())


;;; ordered-container-mixin
;;; the items are in an order, but not sorted (e.g., a list, not a bag)
;;; of course, some of these operations could be very expensive

(defclass* ordered-container-mixin (non-associative-container-mixin)
  ())


;;; sorted-container-mixin

(defclass* sorted-container-mixin (keyed-container-mixin 
                                   ordered-container-mixin)
  ((sorter #'< ia)))


;;; classified-container-mixin
;;;
;;; A classified container determines, based on the key, where an element is
;;; best suited. For example, a quad-tree uses the classifier to choose which
;;; of a node's 4 children an element belongs in. 

(defclass* classified-container-mixin (ordered-container-mixin 
                                       keyed-container-mixin)
  "A classified container returns some value appropriate to the specific
container class. For example, a quad tree is of the type
classified-container-mixin, and the classifier returns one of :TOP-LEFT, 
:TOP-RIGHT, :BOTTOM-LEFT or :BOTTOM-RIGHT"
  ((classifier :initform #'<
               :initarg :classifier
               :accessor classifier)))

;;; uses-contents-mixin
;;;
;;; Container returns its contents via a contents method

(defclass* uses-contents-mixin (abstract-container)
  ((contents nil r)))


(defmethod initialize-instance :after ((object uses-contents-mixin) &rest args 
                                       &key &allow-other-keys)
  (when (or (not (slot-boundp object 'contents))
            (null (contents object))) 
    (setf (slot-value object 'contents) 
          (apply #'make-container-for-contents object args))))


(defmethod collect-elements ((object uses-contents-mixin) &rest args 
                             &key filter transform)
  (declare (ignore filter transform)
           (dynamic-extent args))
  (apply #'collect-elements (contents object) args))


(defmethod iterate-nodes ((object uses-contents-mixin) fn)
  (funcall #'iterate-nodes (contents object) fn))


;;; contents-as-sequence-mixin

(defclass* contents-as-sequence-mixin (uses-contents-mixin
                                         iteratable-container-mixin)
  ())


;;; contents-as-array-mixin
;;;
;;; Underlying structure is an array

(defclass* contents-as-array-mixin (uses-contents-mixin
                                      iteratable-container-mixin)
  ((contents :accessor contents)))


(defmethod size ((container contents-as-array-mixin))
   (total-size container))


(defmethod total-size ((container contents-as-array-mixin))
   (array-total-size (contents container)))


;;; initialize-instance
;;; ---------------------------------------------------------------------------      
;;; So one negative about making an array here is that we have to distinguish
;;; the initial-contents / initial-element case ourselves.  Also, we make
;;; the array 'adjustable'.  This isn't too big a deal since the only method
;;; that relies on the adjustability is 'empty!' and we can easily rewrite it
;;; to test for adjustability, however MCL and ALLEGRO make all arrays
;;; adjustable anyway.

(defmethod insert-initial-contents-p ((container contents-as-array-mixin))
  (values nil))

#+Old
(defmethod initialize-instance :around ((container contents-as-array-mixin)
                                        &key)
  (let ((*insert-initial-contents-p* nil))
    (call-next-method)))


(defmethod make-container-for-contents ((container contents-as-array-mixin)
                                        &rest args)
  (let ((initial-size (getf args :initial-size 0))
        (initial-contents (getf args :initial-contents nil))
        (initial-element (getf args :initial-element nil)) 
        (element-type (getf args :element-type t))
        (fill-pointer (getf args :fill-pointer t)))
    (if initial-contents
      (make-array (length initial-contents)
                  :initial-contents initial-contents
                  :adjustable t
                  :fill-pointer fill-pointer
                  :element-type element-type)
      (make-array initial-size
                  :initial-element initial-element
                  :adjustable t
                  :fill-pointer fill-pointer
                  :element-type element-type))))


;;; Contents-as-list-mixin
;;;
;;; A container whose underlying structure is a list
;;; supports: insert-item, size, empty-p, empty! and iterate-nodes

(defclass* contents-as-list-mixin (keyed-container-mixin 
                                     initial-contents-mixin
                                     ordered-container-mixin
                                     contents-as-sequence-mixin)
  ((contents :initform '()
             :accessor contents)))


(defmethod make-container-for-contents ((object contents-as-list-mixin) &rest args)
  (declare (ignore args))
  (values nil))

(defclass* list-container (contents-as-list-mixin concrete-container)
  ())

;;; contents-as-hashtable-mixin
;;;
;;; A container whose underlying structure is a hashtable
;;; supports: insert-item, size, empty-p, empty! and iterate-nodes

;;?? should we apply the key function to the indexes?
(defclass* contents-as-hashtable-mixin (uses-contents-mixin
                                        key-value-iteratable-container-mixin
                                        findable-container-mixin)
  ((contents :unbound :accessor contents)  ;; unbound for make-ht-for-container
   )) 


;;; Associative-Containers, various flavors

(defclass* associative-container-mixin (initial-contents-key-value-mixin
                                          indexed-container-mixin)
   ())


;;; array-container
;;;
;;; This is a regular array wrapped by the standard associative container
;;; framework.

(defclass* array-container-abstract (associative-container-mixin
                                       bounded-container-mixin
                                       uses-contents-mixin)
  ())


(defclass* array-container (array-container-abstract
                              typed-container-mixin
                              initial-element-mixin
                              iteratable-container-mixin
                              concrete-container)
  ((contents :initform nil
             :accessor contents)))


;;; Sparse-Array-Container
;;;
;;; This uses a hash table and only allocates the memory for
;;; the "cells" of the array that you set.

(defclass* sparse-array-container (associative-container-mixin
                                     contents-as-hashtable-mixin
                                     test-container-mixin
                                     initial-element-mixin
                                     bounded-container-mixin
                                     concrete-container)
  ((dimensions nil ir)
   (use-fixnums? nil r))
  (:default-initargs
    :test #'equal))


;;; simple-associative-container
;;;
;;; like a regular hash-table

(defclass* simple-associative-container (associative-container-mixin
                                           initial-element-mixin
                                           contents-as-hashtable-mixin
                                           test-container-mixin
                                           concrete-container
                                           )
  ())


;;; associative-container
;;;
;;; Nested hash tables

(defclass* associative-container (initial-element-mixin
                                    contents-as-hashtable-mixin
                                    test-container-mixin
                                    associative-container-mixin
                                    concrete-container)
  ())


;;; biassociative-container-mixin
;;;
;;;   A biassociative container is one in which FIND-ITEM and REVERSE-FIND
;;;   are just as fast.  (Positive example: alist, negative example: hashtable).
;;;

(defclass* biassociative-container-mixin (associative-container-mixin)
   ())


;;; Associative-list based container

(defclass* alist-container (biassociative-container-mixin
                              initial-element-mixin
                              key-value-iteratable-container-mixin
                              uses-contents-mixin
                              concrete-container)
  ((test #'equal i)))


;;; stable-associative-container
;;;
;;; an associative container that supports "stable" iteration; i.e., you
;;; can iterate over things in the same order that you put them in. This
;;; is still (Gary King 2003-04-24) a work in progress as it only supports
;;; item-at (setf item-at) and iterate-nodes

(defclass* stable-associative-container (key-value-iteratable-container-mixin
                                           iteratable-container-mixin
                                           associative-container-mixin)
  ((associative-container :unbound)
   (numbered-container :unbound)
   (counter 0 r))
  (:export-p t)
  (:default-initargs
    :container-type 'associative-container))


(defmethod initialize-instance :after ((object stable-associative-container) 
                                       &rest args &key
                                       container-type &allow-other-keys)
  (setf (slot-value object 'associative-container)
        (apply #'make-container container-type :test (test object) args)
        (slot-value object 'numbered-container)
        (make-container container-type :test (test object))))


;;; associative-array

(defclass* associative-array (concrete-container)
  ((dim-container :unbound)
   (num-container :unbound)
   (array-data :unbound r)
   (dimensions :unbound)))


(defmethod initialize-instance :after ((object associative-array) &key 
                                       (test 'eq) (dimensions 2) (max-size 1000))
  (let ((array-dimensions (make-list dimensions
                                     :initial-element max-size)))
    (setf (slot-value object 'dimensions) dimensions 
          (slot-value object 'dim-container)
          (make-instance 'array-container 
            :dimensions max-size
            :initial-element-fn 
            (lambda ()
              (make-container 'associative-container :test test)))
          (slot-value object 'num-container)
          (make-instance 'array-container 
            :dimensions dimensions
            :initial-element 0)
          (slot-value object 'array-data)
          (make-container 'sparse-array-container 
                          :dimensions array-dimensions))))

(defmethod insert-initial-contents-p
    ((container basic-initial-contents-mixin))
  (values t))

(defmethod iteratable-p ((thing iteratable-container-mixin))
  (values t))

(defmethod key-value-iteratable-p
    ((thing key-value-iteratable-container-mixin))
  (values t))
