(in-package #:containers)

;;; just regular lists

(defmethod iteratable-p ((thing list))
  (values t))

;; need both b/c list-container cannot use abstract-containers
(defmethod iterate-elements ((list null) fn)
  (declare (ignore fn)))

(defmethod iterate-nodes ((list null) fn)
  (declare (ignore fn)))

;; need both b/c list-container cannot use abstract-containers
(defmethod iterate-elements ((list list) fn)
  (mapc fn list))

(defmethod iterate-nodes ((list list) fn)
  (mapc fn list))


(defmethod collect-elements ((list list) &key filter transform)
  (cond ((and filter transform)
         (let ((result nil))
           (mapc (lambda (item)
                   (when (funcall filter item)
                     (push (funcall transform item) result)))
                 list)
           (nreverse result)))
        ((not (null filter))
         (let ((result nil))
           (mapc (lambda (item)
                   (when (funcall filter item)
                     (push item result)))
                 list)
           (nreverse result)))
        ((not (null transform))
         (let ((result nil))
           (mapc (lambda (item)
                   (push (funcall transform item) result))
                 list)
           (nreverse result)))
        (t
         (values list)))) 


(defmethod element-position ((list list) element &key (key 'identity) (test 'eq))
  (position element list :key key :test test))


(defmethod empty-p ((list list))
  (null list))


(defmethod first-element ((list list))
  (first list))


(defmethod (setf first-element) (value (list list))
  (setf (first list) value))


(defmethod last-element ((list list))
  (first (last list)))


(defmethod (setf last-element) (value (list list))
  (setf (first (last list)) value))


(defmethod nth-element ((list list) (n integer))
  (nth n list))


(defmethod search-for-match ((list list) predicate &key (key 'identity))
  (%search-for-match list predicate key))


(defmethod size ((list list))
  (length list))


(defmethod find-item ((list list) item)
  (find item list))


(defmethod search-for-item ((list list) item &key test key)
  (find item list :test test :key key))


(defmethod search-for-element ((list list) item &key
                               (test nil) (key 'identity))
  (find item list :test test :key key))


(defmethod reduce-elements ((container list)
                            function
                            &key (key 'identity)  
                            (initial-value nil initial-value-supplied-p)
                            (start 0) end)
  (reduce-internal 
   container #'iterate-elements function key initial-value
   initial-value-supplied-p start end))


(defmethod some-item-p ((container list) (predicate function))
  (%some-thing-p container #'iterate-elements predicate))


(defmethod some-element-p ((container list) predicate)
  (%some-thing-p container #'iterate-elements predicate))


(defmethod every-item-p ((container list) (predicate function))
  (%every-thing-p container #'iterate-elements predicate))


(defmethod every-element-p ((container list) predicate)
  (%every-thing-p container #'iterate-elements predicate))


;;; vectors

(defmethod iterate-elements ((vector vector) fn)
  (iterate-nodes vector fn))


(defmethod iterate-nodes ((vector vector) fn)
  (loop for i across vector
        doing (funcall fn i)))


(defmethod iteratable-p ((thing vector))
  (values t))


(defmethod collect-elements ((vector vector) &key filter transform)
  (loop for element across vector
        when (or (not filter)
                 (funcall filter element))
        collect (if (not transform) element (funcall transform element))))


(defmethod size ((vector vector))
  ;; nb. Since we allow fill pointers now length and array-total-size 
  ;; may be different. The implementation of empty-p and last-element 
  ;; suggest that size should be length.
  (length vector))


(defmethod empty-p ((vector vector))
  (zerop (size vector)))


(defmethod first-element ((vector vector))
  (aref vector 0))


(defmethod (setf first-element) (value (vector vector))
  (setf (aref vector 0) value))


(defmethod last-element ((vector vector))
  (aref vector (1- (size vector))))


(defmethod (setf last-element) (value (vector vector))
  (setf (aref vector (1- (size vector))) value))


(defmethod search-for-match ((vector vector) predicate &key (key 'identity))
  (%search-for-match vector predicate key))


(defmethod some-item-p ((container vector) (predicate function))
  (%some-thing-p container #'iterate-elements predicate))


(defmethod some-element-p ((container vector) predicate)
  (%some-thing-p container #'iterate-elements predicate))


(defmethod every-item-p ((container vector) (predicate function))
  (%every-thing-p container #'iterate-elements predicate))


(defmethod every-element-p ((container vector) predicate)
  (%every-thing-p container #'iterate-elements predicate))


(defmethod sort-elements ((container sequence) sorter
                           &key (key 'identity))
  (sort (copy-seq container) sorter :key key))


(defmethod nth-element ((container vector) (index number))
  (aref container index))


;;; arrays

(defmethod iterate-elements ((array array) fn)
  (maparray array fn))


(defmethod iterate-nodes ((array array) fn)
  (iterate-elements array fn))


(defmethod iteratable-p ((thing array))
  (values t))


(defmethod collect-elements ((array array) &key filter transform)
  (let ((result nil))
    (iterate-elements 
     array 
     (lambda (element)
       (when (or (not filter)
                 (funcall filter element))
         (push (if (not transform) element (funcall transform element))  result))))
    (nreverse result)))


(defmethod size ((array array))
  ;; nb. Since we allow fill pointers now length and array-total-size 
  ;; may be different. The implementation of empty-p and last-element 
  ;; suggest that size should be length.
  (length array))


(defmethod first-element ((array array))
  (row-major-aref array 0))


(defmethod (setf first-element) (value (array array))
  (setf (row-major-aref array 0) value))


(defmethod last-element ((array array))
  (row-major-aref array (size array)))


(defmethod (setf last-element) (value (array array))
  (setf (row-major-aref array (size array)) value))


(defmethod search-for-match ((array array) predicate &key (key 'identity))
  (%search-for-match array predicate key))


(defmethod some-element-p ((array array) predicate)
  (%some-thing-p array #'iterate-elements predicate))


(defmethod every-element-p ((array array) predicate)
  (%every-thing-p array #'iterate-elements predicate))


#+NotYet
(defmethod sort-elements ((container sequence) sorter
                           &key (key 'identity))
  (sort (copy-seq container) sorter :key key))


(defmethod nth-element ((container array) (index number))
  (row-major-aref container index))


(defmethod item-at ((array array) &rest indexes)
  (declare (dynamic-extent indexes))
  (apply #'aref array indexes))


(defmethod item-at! ((array array) value &rest indexes)
  (declare (dynamic-extent indexes))
  (setf (apply #'aref array indexes) value))

;;; hash tables

(defmethod iteratable-p ((thing hash-table))
  (values t))


;; need both b/c hash-tables cannot use abstract-containers
(defmethod iterate-elements ((hash-table hash-table) fn)
  (maphash 
   (lambda (k v)
     (declare (ignore k))
     (funcall fn v))
   hash-table))


(defmethod iterate-nodes ((hash-table hash-table) fn)
  (iterate-elements hash-table fn))


(defmethod collect-elements ((hash-table hash-table) &key filter transform)
  (let* ((result nil)
         (fn (cond ((and filter transform)
                    (lambda (key item)
                      (declare (ignore key))
                      (when (or (not filter)
                                (and filter (funcall filter item)))
                        (push (if transform
                                (funcall transform item)
                                item) result))))
                   ((not (null filter))
                    (lambda (key item)
                      (declare (ignore key))
                      (when (funcall filter item)
                        (push item result))))
                   ((not (null transform))
                    (lambda (key item)
                      (declare (ignore key))
                      (push (funcall transform item) result)))
                   (t
                    (lambda (key item)
                      (declare (ignore key))
                      (push item result))))))
    (maphash fn hash-table)
    (nreverse result)))


(defmethod collect-keys ((hash-table hash-table) &key filter (transform 'identity))
  (let ((result nil))
    (maphash (lambda (k v)
               (declare (ignore v))
               (when (or (not filter)
                         (funcall filter k))
                 (push (funcall transform k) result)))
             hash-table)
    result))

#+Hmmm
(defmethod collect-keys ((hash-table hash-table) &key filter transform)
  (%hash-table-collector (lambda (k v) (declare (ignore v)) k) 
                         (when filter
                           (lambda (k v) (declare (ignore v)) (funcall filter k)))
                         (when transform
                           (lambda (k v) (declare (ignore v)) (funcall transform k)))))

#+Hmmm
(defun %hash-table-collector (hash-table function filter transform)
  (let* ((result nil)
         (fn (cond ((and filter transform)
                    (lambda (key item)
                      (when (or (not filter)
                                (and filter (funcall filter key item)))
                        (push (if transform
                                (funcall transform key item)
                                item) result))))
                   ((not (null filter))
                    (lambda (key item)
                      (when (funcall filter key item)
                        (push item result))))
                   ((not (null transform))
                    (lambda (key item)
                      (push (funcall transform key item) result)))
                   (t
                    (lambda (key item)
                      (push item result))))))
    (maphash fn hash-table)
    (nreverse result)))


(defmethod iterate-key-value ((hash-table hash-table) fn)
  (maphash fn hash-table))
                              

(defmethod empty! ((hash-table hash-table))
  (clrhash hash-table)
  (values))


(defmethod empty-p ((hash-table hash-table))
  (= (size hash-table) 0))


(defmethod size ((hash-table hash-table))
  (hash-table-count hash-table))


(defmethod item-at ((hash-table hash-table) &rest indexes)
  (gethash indexes hash-table))


(defmethod item-at! ((value t) (hash-table hash-table) &rest indexes)
  (setf (gethash indexes hash-table) value))


(defmethod item-at-1 ((hash-table hash-table) index)
  (multiple-value-bind (value found?)
                       (gethash index hash-table)
    (if found?
      (values value t)
      (values nil nil))))


(defmethod item-at-1! ((hash-table hash-table) (value t) index)
  (setf (gethash index hash-table) value))


(defmethod print-container ((hash-table hash-table)
			    &optional (stream *standard-output*))
  (iterate-key-value hash-table 
		     (lambda (k v) (format stream "~&(~s => ~s)" k v))))


;;; alist

(defmethod item-at ((container list) &rest indexes)
  (cdr (assoc (first indexes) container)))


(defmethod item-at-1 ((container list) index)
  (cdr (assoc index container)))


(defmethod iterate-key-value ((container list) function)
  (loop for (k . value) in container 
        when value do
        (funcall function k value))
  #+No
  (loop for keys = container then (rest keys)
        for values = (rest keys) then (rest keys) 
        while keys do
        (funcall function (first keys) (first values))
        (setf keys (rest keys))))


(defmethod collect-key-value ((container list) &rest args &key filter transform)
  (declare (dynamic-extent args)
           (ignore filter transform))
  (apply #'%collect-key-value container args))

(defmethod collect-keys ((container list) &key filter (transform 'identity))
  "If the list begins with an atom, then it is treated as a property list;
otherwise, it is treated as an associative-list." 
  (if (atom (car container))
      ;; treat as property list
      (loop for current in container by #'cddr 
	    when (or (not filter)
		     (funcall filter current)) collect
	   (if transform (funcall transform current) current))
      ;; treat as alist
      (%collect-key-value
       container :filter filter
       :transform (lambda (k v) (declare (ignore v)) (funcall transform k)))))


(defmethod delete-item-at ((container list) &rest indexes)
  (when (assoc (first indexes) container)
    (setf (cdr (assoc (first indexes) container)) nil)))


