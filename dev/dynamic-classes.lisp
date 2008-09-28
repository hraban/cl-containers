;; this is the only part that depends on dynamic-classes

(in-package #:cl-containers)

(defvar *current-iteratee* nil)
  
(defmethod make-container ((classes list) &rest args)
  (let ((name (dynamic-classes:find-or-create-class
	       'abstract-container classes))) 
    (apply #'make-instance name args)))

(dynamic-classes:add-parameter->dynamic-class
 :iterator :transform 'transforming-iterator-mixin)

(dynamic-classes:add-parameter->dynamic-class
 :iterator :filter 'filtered-iterator-mixin)

(dynamic-classes:add-parameter->dynamic-class
 :iterator :unique 'unique-value-iterator-mixin)

(dynamic-classes:add-parameter->dynamic-class
 :iterator :circular 'circular-iterator-mixin)

#+(or)
;;?? Gary King 2005-07-18: didn't work??
(add-dynamic-class-for-parameters :generator 'arithmetic-sequence-generator
                                    nil '(:start :by))

(dynamic-classes:add-parameter->dynamic-class
 :generator :start 'arithmetic-sequence-generator)

(dynamic-classes:add-parameter->dynamic-class
 :generator :by 'arithmetic-sequence-generator)

(dynamic-classes:add-parameter->dynamic-class
 :generator :transform 'transforming-iterator-mixin)

(dynamic-classes:add-parameter->dynamic-class
 :generator :filter 'filtered-iterator-mixin)

(dynamic-classes:add-parameter->dynamic-class
 :generator :unique 'unique-value-iterator-mixin)

(dynamic-classes:add-parameter->dynamic-class
 :generator :end 'finite-arithmetic-sequence-generator)

(defmethod dynamic-classes:existing-subclass
    ((class-type (eql :iterator)) class-list)
  (dynamic-classes:find-existing-subclass 'abstract-generator class-list))

(defmethod dynamic-classes:existing-subclass
    ((class-type (eql :generator)) class-list)
  (dynamic-classes:find-existing-subclass 'abstract-generator class-list))

(defun determine-iterator-class (iteratee iterator-class &rest parameters)
  (let ((*current-iteratee* iteratee))
    (apply #'dynamic-classes:determine-dynamic-class 
           :iterator 
           iterator-class
           parameters)))

(defun determine-generator-class (generator-class &rest parameters)
  (apply #'dynamic-classes::determine-dynamic-class
	 :generator generator-class parameters))

(defmethod dynamic-classes:include-class-dependencies
    ((class-type (eql :iterator)) 
     dynamic-class class-list &rest parameters
     &key treat-contents-as &allow-other-keys)
  (declare (ignore dynamic-class parameters))
  (append class-list 
          (list
           (or
            (and treat-contents-as
                 (class-for-contents-as *current-iteratee* treat-contents-as))
            (base-class-for-iteratee *current-iteratee*)))))

;;;;


(defmethod class-for-contents-as ((contents t) (as t))
  (values nil))
  
(defmethod base-class-for-iteratee ((container list))
  'list-iterator)

(defmethod base-class-for-iteratee ((container array))
  'array-iterator)

(defmethod base-class-for-iteratee ((container hash-table))
  'hash-table-iterator)

(defmethod base-class-for-iteratee ((container uses-contents-mixin))
  (base-class-for-iteratee (contents container)))

