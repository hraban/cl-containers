(in-package #:containers)

(export '(add-index
          delete-index
          insert-record
          delete-record
          update-record
          update-index
          
          table-named
          table-names
          lookup-record
          find-record-if
          column-names
          
          table-container
          empty-tables!))
          
#+Ignore
(declaim (optimize (debug 3)))

;;; biassociative-container
;;;
;;; only good for 1-1 maps at this point

(defclass* biassociative-container (concrete-container
                                    key-value-iteratable-container-mixin
                                    initial-element-mixin
                                    biassociative-container-mixin
                                    test-container-mixin
                                    )
  ((contents-> :unbound r)
   (contents<- :unbound r))
  (:default-initargs 
    :test #'eq))


(defmethod initialize-instance :after ((object biassociative-container) &key test)
  (setf (slot-value object 'contents->) 
        (make-container 'simple-associative-container :test test)
        (slot-value object 'contents<-)
        (make-container 'simple-associative-container :test test)))


(defmethod size ((container biassociative-container))
  (size (contents-> container)))


(defmethod empty! ((container biassociative-container))
  (empty! (contents-> container))
  (empty! (contents<- container))
  (values))


(defmethod iterate-elements ((container biassociative-container) fn)
  (iterate-elements (contents-> container) fn))


(defmethod iterate-keys ((container biassociative-container) fn)
  (iterate-elements (contents<- container) fn))


(defmethod iterate-key-value ((container biassociative-container) function)
  (iterate-key-value (contents-> container) function))


(defmethod iterate-value-key ((container biassociative-container) function)
  (iterate-key-value (contents<- container) function))


(defmethod item-at ((container biassociative-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (item-at-1 (contents-> container) (first indexes)))


(defmethod item-at! ((container biassociative-container) value &rest indexes)
  (setf (item-at-1 (contents-> container) (first indexes)) value
        (item-at-1 (contents<- container) value) (first indexes)))


(defmethod key-at ((container biassociative-container) value)
  (item-at-1 (contents<- container) value))

#+Test
(let ((c (make-container 'biassociative-container)))
  (setf (item-at c :a) 1)
  (setf (item-at c :b) 2)
  (iterate-keys c #'print)
  (iterate-elements c #'print)
  (print (key-at c 2))
  (print (key-at c 1)))


;;; table-container

(defclass* table-container (key-value-iteratable-container-mixin
                              concrete-container)
  ((unique-counter 0 r)
   (contents :unbound r)
   (indexes :unbound r)
   (index :unbound a)
   (prototype :unbound ir)
   (update-queue :unbound r)
   (primary-key :unbound r)))


(defmethod print-object ((container table-container) stream)
  (print-unreadable-object (container stream :type t :identity t)
    (format stream "~A ~D" (class-name (prototype container)) (size container))))


(defmethod initialize-instance :after ((object table-container) &key)
  (setf (slot-value object 'contents)
        (make-container 'biassociative-container)
        (slot-value object 'indexes)
        (make-container 'alist-container)
        (slot-value object 'update-queue) (make-container 'basic-queue)))


(defclass* table-index ()
  ((table nil ir)
   (key nil ir)
   (kind nil ir)
   (test 'eq ir)
   (index nil r)))


(defmethod initialize-instance :after ((object table-index) &key test)
  (setf (slot-value object 'index)
        (make-container 'simple-associative-container :test test)))

(defgeneric add-index (table name index-kind function test)
  (:documentation ""))
(defgeneric delete-index (table name)
  (:documentation ""))
(defgeneric insert-record (table object)
  (:documentation ""))
(defgeneric delete-record (table object)
  (:documentation ""))
(defgeneric update-index (index)
  (:documentation ""))
(defgeneric save-pending-updates (table)
  (:documentation ""))

(defmethod add-index ((object table-container) (name symbol) 
                      (index-kind symbol) key test)
  (setf (item-at (indexes object) name) 
        (make-instance 'table-index :table object
                       :kind index-kind :key key :test test))
  (when (eq index-kind :primary-key)
    (setf (slot-value object 'primary-key) name))
  (update-index (item-at (indexes object) name)))


(defmethod delete-index ((object table-container) (name symbol))
  (delete-item-at (indexes object) name))


(defmethod update-index ((index table-index))
  (let ((index-data (index index))
        (index-key (key index)))
    (iterate-key-value
     (contents (table index))
     (lambda (key element)
       ;;?? not right
       (setf (item-at index-data (funcall index-key element)) key)))))


(defmethod update-index-for-object (index object object-id)
  (setf (item-at (index index) (funcall (key index) object)) object-id))


(defmethod update-indexes-for-object (table object)
  (let ((object-id (key-at (contents table) object)))
    (iterate-elements (indexes table)
                      (lambda (index)
                        (update-index-for-object index object object-id)))))


(defmethod insert-record ((table table-container) object)
  (setf (item-at (contents table) (incf (slot-value table 'unique-counter)))
        object)
  (update-indexes-for-object table object))


(defmethod find-record ((table table-container) (index-name symbol) value)
  (item-at
   (contents table)
   (item-at (index (item-at (indexes table) index-name)) value)))


(defmethod lookup-record ((table table-container) value &optional (error? nil))
  (cond ((find-record table (primary-key table) value))
        ((not error?) (values nil))
        (t
         (error 'record-not-found-error
                :table table
                :value value))))


(defmethod find-record-if ((table table-container) predicate &optional (error? nil))
  (cond ((block searcher
           (iterate-elements
            table
            (lambda (element)
              (when (funcall predicate element)
                (return-from searcher element))))))
        ((not error?) (values nil))
        (t
         ;;?? this is a bit wonky
         (error 'record-not-found-error
                :table table
                :value predicate))))
  

(defmethod size ((container table-container))
  (size (contents container)))


(defmethod iterate-container ((container table-container) fn)
  (iterate-elements (contents container) fn))

  
#+Test
(defclass* foo (u:numbered-instances-mixin)
  ((value nil ia)))
#+Test
(let ((c (make-container 'table-container)))
  (add-index c 'id 'u:object-number)
  (add-index c 'value 'value)
  (insert-record c (make-instance 'foo :value 1))
  (insert-record c (make-instance 'foo :value 2))
  (insert-record c (make-instance 'foo :value 3))
  c)


(defmethod column-names ((container table-container))
  (mopu:slot-names (prototype container)))


(defmethod empty! ((container table-container))
  (empty! (contents container))
  (empty! (update-queue container))
  (setf (slot-value container 'unique-counter) 0))


;;; "database"

(defclass* database-mixin ()
  ((database-tables :unbound r))
  :export-slots
  (:export-p t))


(defmethod initialize-instance :after ((object database-mixin) &key)
  (setf (slot-value object 'database-tables)
        (make-container 
         'alist-container
         :initial-element-fn (lambda ()
                               (make-container 'associative-container))))) 

(defmethod table-named ((database database-mixin) (table symbol))
  (item-at (database-tables database) table))


(defmethod (setf table-named) ((value table-container) 
                               (database database-mixin) (table symbol))
  (setf (item-at (database-tables database) table) value))


(defgeneric table-names (database)
  (:documentation "Returns a list of the names of the tables in a database.")
  (:method ((database database-mixin))
           (collect-keys (database-tables database))))


(defmethod empty! ((database database-mixin))
  (empty! (database-tables database)))


(defmethod empty-tables! ((database database-mixin))
  (iterate-elements
   (database-tables database)
   #'empty!))

;;; a bit wonky

(defun apply-filter-to-database (database filter)
  (iterate-elements 
   (database-tables database)
   (lambda (table)
     (unless (member 'filtered-container-mixin 
                     (mopu:superclasses table)
                     :key #'class-name)
       (change-class 
        table 
        (find-matching-container-class (list (type-of table) 'filtered-container-mixin))))
     (setf (element-filter table) filter))))

#+Test
(iterate-elements 
   (database-tables (ib::information-broker))
   (lambda (table)
     (change-class 
      table 'table-container)))


(defun print-schema (database)
  (iterate-key-value 
   (database-tables database)
   (lambda (name table)
     (format t "~%~A~%  ~{~A~^, ~}" name (column-names table)))))

