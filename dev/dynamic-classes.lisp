(in-package #:cl-containers)

#|
pulled in from separate library 'cause it was just easier, dammit
|#

;;; some class defining functions

(defvar *define-class-form* 'metatilities:defclass*
  "The name of the form used to define a class. Usually, this will be bound to 'defclass* but when we are using GBBOpen, it will probably be bound to define-class or define-class*.")

#+test
(setf *define-class-form* 'metatilities:defclass*)

(defun simple-define-class 
    (superclasses 
     &optional (name (simple-define-class-name superclasses)))
  "Define a class on the fly..."
  (cond ((and (length-1-list-p superclasses)
               (find-class (first superclasses) nil))
         (values (first superclasses)))
        (t
	 (muffle-redefinition-warnings
           (eval `(progn
                    (when (find-class ',name nil)
                      (setf (find-class ',name) nil))
                    (defclass* ,name ,(ensure-list superclasses) nil))))
         (values name))))

(defun simple-define-class-name (superclasses &optional (package *package*)) 
  (intern (format nil "~{~a~^-AND-~}" superclasses) package))

(defun define-class (class-name superclasses slots &rest class-options)
  "Define a class with all the bells and whistles on the fly... See 
simple-define-class for the simpler version."
  (muffle-redefinition-warnings
    (eval `(,*define-class-form* 
            ,(or class-name 
                 (setf class-name
                       (simple-define-class-name (ensure-list superclasses))))
             ,(ensure-list superclasses) 
             (,@(ensure-list slots))
             ,@class-options)))
  (values class-name))

(defun map-subclasses (class fn &key proper?)
  "Applies fn to each subclass of class. If proper? is true, then
the class itself is not included in the mapping. Proper? defaults to nil."
  (let ((mapped (make-hash-table :test #'eq)))
    (labels ((mapped-p (class)
               (gethash class mapped))
             (do-it (class root)
               (unless (mapped-p class)
                 (setf (gethash class mapped) t)
                 (unless (and proper? root)
                   (funcall fn class))
                 (mapc (lambda (class)
                         (do-it class nil))
                       (class-direct-subclasses class)))))
      (do-it (get-class class) t))))

(defun superclasses (thing &key (proper? t))
  "Returns a list of superclasses of thing. Thing can be a class, object or symbol naming a class. The list of classes returned is 'proper'; it does not include the class itself."
  (let ((result (class-precedence-list 
		 (finalize-class-if-necessary (get-class thing)))))
    (if proper? (rest result) result)))

(defun find-existing-subclass (superclass superclasses)
  "Look through all the sub-classes of superclass and see if any of them descend
from every class in superclasses."
  (let ((results nil))
    (map-subclasses
     superclass
     (lambda (subclass)
       (let ((last-position -1))
         (when (every (lambda (superclass)
                        (let ((pos
                               (position 
                                superclass (superclasses subclass :proper? nil)
                                :key (lambda (x) (class-name x)))))
                          (prog1
                            (and pos (< last-position pos))
                            (setf last-position pos))))
                      superclasses)
           (push (class-name subclass) results)))))
    (values (first results))))

(defun find-or-create-class (root classes)
  "Try to find a class which is a subclass of root and all of the other `classes` as well. If no such class exists, then it will be created and returned."
  (or (find-existing-subclass root classes)
      (let ((superclasses (remove-redundant-classes classes)))
        (define-class (simple-define-class-name 
		       (remove-redundant-classes superclasses))
          classes nil))))

(defun remove-redundant-classes (classes)
  (loop for class in classes 
        unless (class-redundant-p class classes) collect
        class))

(defun class-redundant-p (class classes)
  (some
   (lambda (other-class)
     (and (not (eq class other-class))
          (subtypep other-class class)))
   classes))

;;;;

(defgeneric include-class-dependencies
    (class-type dynamic-class class-list &rest parameters)
  (:documentation ""))

(defgeneric existing-subclass (class-type class-list)
  (:documentation ""))

;;; Support for dynamic classes based on the parameters for instantiation...
;;;
;;; Here is a quick history lesson: we've been doing this for shapes, since
;;; there was a massive amount of potential shape superclasses, and only a
;;; small subset were ever used for any given instance, and this was the 
;;; cleanest and cutest approach...

(defvar *parameter-dynamic-class-table* nil)

(defun type->parameter-table (type)
  (cdr (assoc type *parameter-dynamic-class-table*)))

(defun (setf type->parameter-table) (value type)
  (let ((it (assoc type *parameter-dynamic-class-table*)))
    (if it
      (setf (cdr it) value)
      (setf *parameter-dynamic-class-table*
            (append *parameter-dynamic-class-table* (list (cons type value))))))
  (values value))

(defun parameter->dynamic-class (table parameter)
  (cdr (assoc parameter table)))

(defun (setf parameter->dynamic-class) (value table parameter)
  (let ((it (assoc parameter table)))
    (if it
      (setf (cdr it) value)
      (let ((temp (cdr table))
            (insert (list (cons parameter value))))
        (setf (cdr insert) temp
              (cdr table) insert))))
  (values value))

(defun table&parameter->dynamic-class (class-type parameter)
  (parameter->dynamic-class (type->parameter-table class-type) parameter))

(defun add-parameter->dynamic-class (class-type 
                                     parameter &rest super-classes)
  (let* ((current-table (or (type->parameter-table class-type) 
                            (list (cons :remove :remove))))
         (have-table? (not (eq (caar current-table) :remove))))
    (dolist (super-class (ensure-list super-classes))
      (let ((it (parameter->dynamic-class current-table parameter)))
        (if it
          (pushnew super-class it)
          (setf (parameter->dynamic-class current-table parameter)
                (list super-class)))))
    (unless have-table?
      (setf (type->parameter-table class-type) current-table)))
  
  (values nil))

(defun add-dynamic-class-for-parameters (class-type dynamic-class 
                                                    &rest parameters)
  (dolist (parameter (ensure-list parameters))
    (add-parameter->dynamic-class 
     class-type parameter dynamic-class)))

#+Later
(defun remove-parameter->dynamic-class (class-type parameter dynamic-class)
  (let ((primary-table 
	 (containers:item-at *parameter-dynamic-class-table* class-type)))
    (when (and primary-table (containers:item-at primary-table parameter))
      (setf (containers:item-at primary-table parameter)
            (remove dynamic-class
		    (containers:item-at primary-table parameter))))))

(defun empty-add-parameter->dynamic-class (class-type)
  (setf (type->parameter-table class-type) nil))

(defun empty-all-add-parameter->dynamic-class ()
  (setf *parameter-dynamic-class-table* nil))

(defun dynamic-class-information ()
  (loop for (type . data) in *parameter-dynamic-class-table* collect
        (list type
              (loop for (parameter . class) in data collect
                    (list parameter class)))))

(defmethod include-class-dependencies 
    ((class-type (eql nil)) 
     dynamic-class class-list &rest parameters)
  (declare (ignore dynamic-class class-list parameters)))

(defmethod existing-subclass ((class-type (eql nil)) (class-list t))
  (values nil))

(defun determine-dynamic-class (class-type dynamic-class &rest parameters)
  (let ((class-list
         (loop for parameter in parameters
               for keyword? = t then (not keyword?)
               when keyword? nconc
               (loop for class in (table&parameter->dynamic-class
				   class-type parameter)
                     when (or (not dynamic-class)
                              (and dynamic-class
                                   (not (subtypep class dynamic-class))))
                     collect class))))
    (setf class-list
          (apply #'include-class-dependencies 
		 class-type dynamic-class class-list parameters))
    (when (and dynamic-class (not (some (lambda (class-name)
                                          (subtypep dynamic-class class-name))
                                        class-list)))
      (setf class-list (nconc (list dynamic-class) class-list)))
    
    (setf class-list (delete-duplicates class-list))
    (let ((it nil))
      (cond ((setf it (existing-subclass class-type class-list))
             it)
            (t
             (if (and (length-1-list-p class-list)
                      (find-class (first class-list) nil))
               (first class-list)
               (define-class nil class-list nil)))))))
