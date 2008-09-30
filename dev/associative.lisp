(in-package #:containers)


;;; Associative-Containers, various flavors

(defmethod some-key-value-p ((container associative-container-mixin) 
                             (predicate t))
  (iterate-key-value container
                     (lambda (k v)
                       (let ((it (funcall predicate k v)))
                         (when it
                           (return-from some-key-value-p it)))))
  
  (values nil))


(defmethod every-key-value-p ((container associative-container-mixin) 
                              (predicate t))
  (iterate-key-value container
                     (lambda (k v)
                       (unless (funcall predicate k v)
                         (return-from every-key-value-p nil))))
  
  (values t))


(defmethod samep ((c1 associative-container-mixin) 
                    (c2 associative-container-mixin))
  (block test
    (iterate-key-value c1
                       (lambda (k v)
                         (unless (eq (item-at c2 k) v)
                           (return-from test nil))))
    (iterate-key-value c2
                       (lambda (k v)
                         (unless (eq (item-at c1 k) v)
                           (return-from test nil))))
    (values t)))


(defmethod collect-keys ((container associative-container-mixin)
                         &key (filter nil) (transform 'identity))
  (collect-key-value container 
                     :filter (when filter (lambda (k v)
                                            (declare (ignore v))
                                            (funcall filter k)))
                     :transform (lambda (k v) 
                                  (declare (ignore v))
                                  (funcall transform k))))


(defmethod search-for-key ((container associative-container-mixin) key-to-find
                           &key test key)
  (iterate-keys container (lambda (k)
                            (when (funcall (or test #'samep) 
                                           (or (and key (funcall key k)) k)
                                           key-to-find)
                              (return-from search-for-key k))))
  (values nil)) 


(defmethod remove-items-if ((container iteratable-container-mixin) test)
  (iterate-elements 
   container
   (lambda (element)
     (when (funcall test element)
       (delete-item container element))))
  (values container))


(defmethod remove-items-if ((container associative-container-mixin) test)
  (iterate-key-value
   container
   (lambda (key value)
     (when (funcall test value)
       (delete-item-at container key))))
  (values container))
                               

(defmethod count-items ((container iteratable-container-mixin) item &key 
                        (key 'identity) (test 'eql))
  (let ((result 0))
    (iterate-nodes 
     container
     (lambda (item2)
       ;; putting item first is consistent with CL test functions
       (when (funcall test item (funcall key item2))
         (incf result))))
    (values result)))


(defmethod count-elements ((container iteratable-container-mixin) item &key 
                           (key 'identity) (test 'eql))
  (let ((result 0))
    (iterate-elements 
     container
     (lambda (item2)
       ;; putting item first is consistent with CL test functions
       (when (funcall test item (funcall key item2))
         (incf result))))
    (values result)))


(defmethod count-elements ((container list) item &key 
                           (key 'identity) (test 'eql))
  (let ((result 0))
    (mapc 
     (lambda (item2)
       ;; putting item first is consistent with CL test functions
       (when (funcall test item (funcall key item2))
         (incf result)))
     container)
    (values result)))


(defmethod count-elements-if ((container iteratable-container-mixin) test &key 
                              (key 'identity))
  (let ((result 0))
    (iterate-elements 
     container
     (lambda (item)
       ;; putting item first is consistent with CL test functions
       (when (funcall test (funcall key item))
         (incf result))))
    (values result)))


(defmethod count-elements-if ((container list) test &key (key 'identity))
  (count-if test container :key key))


;;; array-container
;;;
;;; This is a regular array wrapped by the standard associative container
;;; framework.

(defmethod print-container-summary ((container array-container) stream)
  (format stream "~{~Dx~}" (array-dimensions (contents container))))


(defun make-array-container (dimensions 
                             &key element-type initial-element)
  (make-instance 'array-container
    :dimensions (ensure-list dimensions)
    :initial-element initial-element
    :element-type element-type))


(defmethod make-container-for-contents ((container array-container) &rest args)
  (let ((dimensions (getf args :dimensions))
        (initial-element (getf args :initial-element))
        (element-type (getf args :element-type)))
    (when (initial-element-fn container)
      (setf initial-element +empty-initial-element+))
    (make-array dimensions
                :initial-element initial-element
                :element-type (or element-type t)
                :adjustable t)))


(defmethod make-container ((class (eql 'array-container)) &rest args)
   (let ((dimensions (getf args :dimensions))
         (size (getf args :size)))
     (remf args :dimensions)
     (remf args :size)
     (apply #'make-array-container
            (or dimensions size)
            args)))


(defmethod item-at ((container array-container) &rest indexes)
   (declare (dynamic-extent indexes))
   (let ((result (apply #'aref (contents container) indexes)))
     (if (and (eq result +empty-initial-element+)
              (initial-element-fn container))
       (setf (apply #'item-at container indexes)
             (funcall (initial-element-fn container)))
       result)))


(defmethod item-at! ((container array-container) value &rest indexes)
   (declare (dynamic-extent indexes))
   (setf (apply #'aref (contents container) indexes) value))


(defmethod dimensions ((container array-container))
   (array-dimensions (contents container)))


(defmethod iterate-nodes ((container array-container) fn)
  (dotimes (i (array-total-size (contents container)))
    (funcall fn (row-major-aref (contents container) i))))


(defmethod nth-element ((container array-container) (index integer))
  (nth-element (contents container) index))


;;; Sparse-Array-Container
;;;
;;; This uses a hash table and only allocates the memory for
;;; the "cells" of the array that you set.

(defmethod initialize-instance :around ((object sparse-array-container) &key
                                        dimensions)
  (setf (slot-value object 'dimensions) (ensure-list dimensions))
  (setf (slot-value object 'use-fixnums?) (< (total-size object) most-positive-fixnum))
  (call-next-method))


(defmethod make-container-for-contents ((container sparse-array-container) 
                                        &rest args)
  (declare (ignore args))
  (setf (slot-value container 'test) (if (use-fixnums? container) #'eq #'equal))
  (call-next-method))


(defmethod total-size ((container sparse-array-container))
   (reduce #'* (dimensions container)))


(defun sparse-array-row-major-index (dims subscripts)
  (declare (dynamic-extent dims subscripts))
  (let ((val 0))
    (loop for subscript in subscripts
          for dimension in dims
          do (setf val (+ (* val dimension) subscript)))
    val))

  
(defun sparse-array-value-to-index (dims value)
  (let ((val value))
    (reverse
     (loop for dimension in (reverse dims)
          collect (mod val dimension)
          do (setf val (floor (/ val dimension)))))))

#+TESTING
(addtest (sparse-array-index-conversion)
  (loop for x from 1 to 5
        do
        (loop for y from 1 to 10
              do
              (loop for z from 1 to 20
                    do (ensure (equal (sparse-array-value-to-index 
                                    '(5 10 20)
                                    (sparse-array-row-major-index '(5 10 20) (list x y z)))
                                   (list x y z)))))))

(defmethod item-at ((container sparse-array-container) &rest indexes)
   (declare (dynamic-extent indexes))
   (multiple-value-bind (value found)
                        (if (use-fixnums? container)
                          (gethash (sparse-array-row-major-index
                                    (dimensions container)
                                    indexes)
                                   (contents container))
                          (gethash  indexes (contents container)))
     (if found
       value
       (maybe-set-initial-element container #'item-at! indexes))))


(defmethod item-at! ((container sparse-array-container) value &rest indexes)
   ;; (declare (dynamic-extent indexes))
   (if (use-fixnums? container)
     (setf (gethash (sparse-array-row-major-index
                     (dimensions container)
                     indexes)
                    (contents container))
                    value)
     (setf (gethash indexes (contents container)) value)))


;;; simple-associative-container
;;;
;;; like a regular hash-table

(defmethod item-at-1 ((container simple-associative-container) index)
  (multiple-value-bind (value found?)
                       (gethash index (contents container))
    (if found?
      (values value t)
      (maybe-set-initial-element container #'item-at-1! (list index)))))


(defmethod item-at-1! ((container simple-associative-container) value index)
  (setf (gethash index (contents container)) value))


(defmethod (setf item-at-1) (value container index)
  (item-at-1! container value index))


(defmethod item-at ((container simple-associative-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (assert (length-at-most-p indexes 1)) 
  (item-at-1 container (first indexes)))


(defmethod item-at! ((container simple-associative-container) value &rest indexes)
  (assert (length-at-most-p indexes 1)) 
  (setf (item-at-1 container (first indexes)) value))


(defmethod delete-item-at ((container simple-associative-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (assert (length-at-most-p indexes 1)) 
  (remhash (first indexes) (contents container)))


(defmethod iterate-key-value ((container simple-associative-container) function)
  (maphash function (contents container)))


;;; associative-container
;;;
;;; Nested hash tables

(defmethod initialize-instance :after ((object associative-container) 
                                       &key (test 'eq))
  (assert (member (typecase test
                    (function test)
                    (symbol (symbol-function test)))
                  (list #'eq #'eql #'equal))
          nil "Test function must be one of #'eq, #'eql or #'equal"))


(defmethod item-at ((container associative-container) &rest indexes)
   (multiple-value-bind (value found?)
                        (descend-ht (contents container) indexes)
     (if found?
       (values value t)
       (maybe-set-initial-element container #'item-at! indexes))))


#+WIP
(defmethod item-at ((container associative-container) &rest indexes)
   (declare (dynamic-extent indexes))
   (multiple-value-bind (ht last-index)
                        (find-or-create-ht
                         (contents container)
                         indexes
                         (lambda () (make-container-for-contents container)))
     (gethash last-index ht)))


(defmethod delete-item-at ((container associative-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (multiple-value-bind (ht last-index)
                       (find-or-create-ht
                        (contents container)
                        indexes
                        #'(lambda () (make-container-for-contents container)))
    (remhash last-index ht))
  (values container))


;; Shouldn't there be simpler version of this for the 'contents-as-hashtable-mixin' class
;; that just calls maphash to map the function over the contents - Westy
(defmethod iterate-key-value ((container contents-as-hashtable-mixin) function)
  (labels ((collect-it (k v fn &optional (collected-keys nil))
             (if (typep v 'hash-table)
               (descend v fn (append collected-keys (list k)))
               (funcall fn (if collected-keys
                             (append collected-keys (list k))
                             k) v)))
           (descend (ht fn &optional (collected-keys nil))
             (maphash #'(lambda (k v)
                          (collect-it k v fn collected-keys))
                      ht)))
    (descend (contents container) function)))


;;?? this would be weird if you stored HT's in your associative container

;; If we just went by the index-list, then it would be okay, however,
;; you could get into trouble by giving a bogus last argument...

(defun descend-ht (ht index-list)
   (assert (not (null index-list)) nil "Index list must be non-null.")

   (multiple-value-bind (value found?)
                        (gethash (first index-list) ht)
     (if found?
       (if (typep value 'hash-table)
         (descend-ht value (rest index-list))
         (values value t))
       (values nil nil))))


(defun find-or-create-ht (ht index-list create-fn)
   ;(spy ht index-list)
   (assert (not (null index-list)) nil "Index list must be non-null.")

   (multiple-value-bind (value found?)
                        (gethash (first index-list) ht)
     (unless (and found?
                  (typep value 'hash-table))
       (setf value (setf (gethash (first index-list) ht)
                         (funcall create-fn))))

     (cond ((null (rest index-list))
            (values ht (first index-list)))
           ((length-1-list-p (rest index-list))
            (values value (first (rest index-list))))
           (t
            (find-or-create-ht value (rest index-list) create-fn)))))

#+Test
(let* ((mk-fn (lambda () (make-hash-table)))
        (ht (funcall mk-fn))
        (value 5))
   (multiple-value-bind (ht last-index)
                        (find-or-create-ht ht '(:a :b :c) mk-fn)
     (spy ht last-index)
     (setf (gethash last-index ht) value))
   ht)


(defmethod item-at! ((container associative-container) value &rest indexes)
   (multiple-value-bind (ht last-index)
                        (find-or-create-ht
                         (contents container)
                         indexes
                         #'(lambda () (make-container-for-contents container)))
     (setf (gethash last-index ht) value)))


(defmethod print-container ((container associative-container)
                            &optional (stream *standard-output*))
  (iterate-key-value container
                     (lambda (k v)
                       (format stream "~&(~s => ~s)" k v)))
  (values container))


(defmethod print-container ((container stable-associative-container)
                            &optional (stream *standard-output*))
  (iterate-key-value-stably container
                            (lambda (k v)
                              (format stream "~&(~s => ~s)" k v)))
  (values container))


;;; Associative-list based container

(defmethod initialize-container ((container alist-container))
   (setf (slot-value container 'contents) nil))


(defmethod make-container-for-contents ((container alist-container) &rest args) 
  (declare (ignore args))
  nil)


#+Ignore 
;; Clayton Morrison 2005-03-23: bug: alist indexes like '(b a) may
;; make it impossible to find items indexed by '(b)...
(defmethod item-at ((container alist-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (let* ((key (if (length-1-list-p indexes) #'car #'identity))
         (real-indexes (if (length-1-list-p indexes) (first indexes) indexes))
         (it (cdr (assoc real-indexes (contents container) 
                         :test (test container) :key key))))
    (if it
      (values it t)
      (maybe-set-initial-element container #'item-at! indexes))))

;; only compares first of indexes and alist-key if _both_ are length-1 lists
(defmethod item-at ((container alist-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (let* ((it (cdr (assoc indexes (contents container) 
                         ;; in an assoc predicate, the first arg is always
                         ;;  the key (in this case, indexes)
                         :test (lambda (key1 key2)
                                 (if (and (length-1-list-p key1)
                                          (length-1-list-p key2))
                                   (funcall (test container) (first key1) (first key2))
                                   (funcall (test container) key1 key2)))))))
    (if it
      (values it t)
      (maybe-set-initial-element container #'item-at! indexes))))


(defmethod item-at-1 ((container alist-container) index)
  (let* ((it (cdr (assoc index (contents container) 
                         :test (lambda (key1 key2)
                                 (funcall (test container) key1 key2))))))
    (if it
      (values it t)
      (maybe-set-initial-element container #'item-at-1! (list index)))))


(defmethod item-at-1! ((container alist-container) value index)
  (let* ((item (assoc index (contents container) 
                      :test (test container) :key #'identity)))
    (if item
      (setf (cdr item) value)
      (progn
        (setf item (cons index value))
        (push item (slot-value container 'contents))))
    
    value))


(defun maybe-set-initial-element (container setter-fn indexes)
  (if (has-initial-element-p container)
    (let ((ie (make-initial-element container)))
      (unless (eq ie +empty-initial-element+)
        (apply setter-fn container ie indexes)
        (values ie nil)))
    (values nil nil)))


(defmethod item-at! ((container alist-container) value &rest indexes)
  (let* ((key (if (length-1-list-p indexes) #'car #'identity))
         (real-indexes (if (length-1-list-p indexes) (first indexes) indexes))
         (item (assoc real-indexes (contents container) 
                      :test (test container) :key key)))
    (if item
      (setf (cdr item) value)
      (progn
        (setf item (cons indexes value))
        (push item (slot-value container 'contents))))
    
    value))


(defmethod delete-item-at ((container alist-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (setf (slot-value container 'contents)
        (delete indexes (slot-value container 'contents)
                :test (test container) :key #'first))
  (values container))


;;?? Does this make sense
(defmethod size ((container alist-container))
   (let ((result 0))
     (dolist (assoc (contents container))
       (when (cdr assoc)
         (incf result)))
     result))


(defmethod iterate-nodes ((container alist-container) function)
   (dolist (assoc (contents container))
       (when (cdr assoc)
         (funcall function (cdr assoc)))))


(defmethod iterate-keys ((container alist-container) function)
   (dolist (assoc (contents container))
     (when (cdr assoc)
       (if (length-1-list-p (first assoc))
         (funcall function (caar assoc))
         (funcall function (first assoc))))))


(defmethod reverse-find ((container alist-container) value &key (test 
                                                                 (test container)))
  (car (rassoc value (contents container)
               :test test)))


(defmethod empty! ((container alist-container))
   (initialize-container container)
   (values nil))


(defmethod iterate-key-value ((container alist-container) function)
  (loop for (k . value) in (contents container) 
        when value do
        (funcall function (if (length-1-list-p k) (first k) k) value)))


(defmethod sort-keys ((container alist-container) sorter &key key)
  (sort-container container sorter
                  (if key
                    (lambda (element)
                      (funcall key (caar element)))
                    #'caar))
  (values container))


(defmethod sort-elements ((container alist-container) sorter &key key)
  (sort-container container sorter
                  (if key
                    (lambda (element)
                      (funcall key (cdr element)))
                    #'cdr))
  (values container))


(defmethod sort-container ((container uses-contents-mixin) sorter key-fn)
  (setf (slot-value container 'contents) 
        (sort (contents container) sorter :key key-fn))
  (values container))


;;; keyed-associative-container
;;;
;;; This container has two types of keys.  The 'keyed' in
;;; keyed-associative-container specifies a key function applied to the
;;; key of a key/value pair.  This key function provides a way to transform
;;; the key into a thing that can be compared with eq, eql, or equal. In
;;; effect, this lets you have hashtables with arbitrary tests for equality.
;;; If there are multiple indexes, the key is applied to the arguments as a
;;; whole, and is assumed to return multiple-values.  That is, the key should
;;; have arity equal to the dimensions of the container, and should return
;;; an equal number of values.
;;;
;;; This functionality is similar to the hashCode() style function required
;;; in java hashtables in that the 'key' function returns a hash, or more
;;; appropriately, something that can be hashed by the built in s
;;;
;;; Importantly, the original key is used for all iterating procedures, so
;;; one can expect key instead of key-function(key).  Finally, note that the
;;; key function is applied to each of the index keys seperately, and not
;;; as a whole.

(defclass* keyed-associative-container (associative-container keyed-container-mixin)
   ((key-map :accessor key-map
             :type associative-container)
    (in-item-at? nil ir))
   (:default-initargs
     :test #'eq
     :key #'identity-2))

;;; initialize-instance :: keyed-associative-container -> nil
;;; We need to initialize the key-map here because we need the test function
;;; given for the underlying hashtable.  We could just get it directly from
;;; the keyword :test, but this will suffice

(defmethod initialize-instance :after ((container keyed-associative-container) &key)
   (setf (key-map container)
         (make-container 'associative-container :test (test container))))

;;?? Gary King 2003-08-13: kill these someday
;;; singleton-or-list
;;; If the list 'lst' contains only one item, then return it, otherwise
;;; return the original list

(defun identity-2 (&rest lst)
  (if (length-1-list-p lst) 
    (first lst)
    lst))


;;; item-key :: keyed-associative-container -> ... -> t
(defmethod item-key ((container keyed-associative-container) &rest indexes)
   (apply (key container) indexes))


(defmethod item-at :around ((container keyed-associative-container) 
                            &rest indexes)
  (declare (dynamic-extent indexes))
  (cond ((in-item-at? container)
         (call-next-method))
        (t
         (unwind-protect
           (progn
             (setf (slot-value container 'in-item-at?) t)
             (multiple-value-call #'call-next-method container
                                  (apply #'item-key container indexes)))
           (setf (slot-value container 'in-item-at?) nil)))))


(defmethod delete-item-at :around ((container keyed-associative-container) 
                                   &rest indexes)
  (declare (dynamic-extent indexes))
  (cond ((in-item-at? container)
         (call-next-method))
        (t
         (unwind-protect
           (progn
             (setf (slot-value container 'in-item-at?) t)
             (let ((key-list (multiple-value-list (apply #'item-key container indexes))))
               (apply #'call-next-method container key-list)
               (apply #'delete-item-at (key-map container) key-list)))
           (setf (slot-value container 'in-item-at?) nil))))
  container)


(defmethod item-at! :around ((container keyed-associative-container) 
                             value &rest indexes)
  (declare (dynamic-extent indexes))
  (cond ((in-item-at? container)
         (call-next-method))
        (t
         (unwind-protect
           (progn
             (setf (slot-value container 'in-item-at?) t)
             (let ((key-list (multiple-value-list 
                              (apply #'item-key container indexes))))
               (apply #'call-next-method container value key-list)
               (apply #'item-at! (key-map container) indexes key-list)))
           (setf (slot-value container 'in-item-at?) nil))))
  value)


;;; iterate-key-value ::
;;;   keyed-associative-container -> function -> keyed-associative-container
;;; Because we want the original key, and not the key-function key, we must
;;; compute a closure around fn and our key-map

;;?? GWK, I'm not sure I understand this. If multiple keys have the same
;; key-function, then how can we use the original key?

#+Ignore
(defmethod iterate-key-value :around ((container 
                                       keyed-associative-container) fn)
  (call-next-method container #'(lambda (&rest args)
                                  (apply fn
                                         (append
                                          (apply #'item-at (key-map container)
                                                 (butlast args))
                                          (last args))))))


#+Test
(let ((c (make-container 'keyed-associative-container
                          :test #'eq
                          :key #'oddp
                          :initial-element 0)))
   (dolist (elt '(1 2 3 4 5 6 7 7 8 9 10))
     (incf (item-at c elt)))
   (iterate-key-value c
                      (lambda (k v)
                        (format t "~%~A -> ~A~&" k v))))


(defgeneric key-exists-p (container &rest indexes)
  (:documentation ""))


#+Remove
;; this one fails if :initial-element or :initial-element-fn
(defmethod key-exists-p ((container associative-container-mixin) &rest indexes)
  (declare (dynamic-extent indexes))
  (multiple-value-bind (value found?) (apply #'item-at container indexes)
    (declare (ignore value))
    found?))

;;?? UCK
(defmethod key-exists-p ((container associative-container-mixin) &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((old-ie (if (slot-boundp container 'initial-element) 
                  (initial-element container) :unbound))
        (old-ief (if (slot-boundp container 'initial-element-fn)
                   (initial-element-fn container) :unbound)))
    (unwind-protect
      (progn
        (setf (slot-value container 'initial-element) +empty-initial-element+
              (slot-value container 'initial-element-fn) +empty-initial-element+)
        (multiple-value-bind (value found?) (apply #'item-at container indexes)
          (declare (ignore value))
          found?))
      
      (unless (eq old-ie :unbound)
        (setf (slot-value container 'initial-element) old-ie))
      
      (unless (eq old-ief :unbound)
        (setf (slot-value container 'initial-element-fn) old-ief)))))


(defmethod iterate-key-value ((container (eql nil)) fn)
  (declare (ignore fn))
  (values container))