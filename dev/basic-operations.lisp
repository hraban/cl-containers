
(in-package #:containers)

;;; stuff and nonsense

(defgeneric make-container (class &rest args)
  (:documentation "Creates a new container of type class using the additional
arguments (args).")
  (:method ((class symbol) &rest args)
           (apply #'make-instance class args))) 

(defgeneric empty-p (abstract-container)
  (:documentation "Returns t if there are no items in the container.")
  (:method ((container t))
           (zerop (size container))))


(defmethod iterate-elements ((container abstract-container) fn)
  (iterate-nodes container fn))


(defmethod iterate-elements ((container container-uses-nodes-mixin) fn) 
  (iterate-nodes container
                     (lambda (x)
                       (funcall fn (element x)))))


(defmethod find-element ((container container-uses-nodes-mixin) (thing t))
  (let ((node (find-node container thing)))
    (when node (element node))))


(defmethod delete-item ((container container-uses-nodes-mixin) (item t))
  (let ((it (search-for-node container item)))
    (when it
      (delete-item container it))))


(defmethod delete-element ((container container-uses-nodes-mixin) (thing t))
  (delete-item container thing))


(defmethod print-container ((container iteratable-container-mixin) 
                            &optional (stream *standard-output*))
  (iterate-elements container
                    #'(lambda (item)
                        (print item stream)))
  (values container))


(defmethod nth-element :around ((container t) (index integer))
  (if (< -1 index (size container))
    (call-next-method)
    (error 'index-out-of-range-error :container container
           :index index)))


(defmethod nth-element ((container iteratable-container-mixin) (index integer))
  ;; possibly slow but servicable method
  (iterate-elements 
   container
   (lambda (elt)
     (when (minusp (decf index))
       (return-from nth-element elt))))
  (error "Index ~D out of range for container ~A" index container))


(defmethod collect-nodes ((container iteratable-container-mixin) 
                          &key filter transform)
  (collector-internal container #'iterate-nodes filter transform))


(defmethod collect-elements ((container iteratable-container-mixin) 
                             &key filter transform)
  (collector-internal container #'iterate-elements filter transform))


(defun collector-internal (container iterate-fn filter-fn transform-fn)
  (let ((result nil))
    (funcall iterate-fn container
             (lambda (item)
               (when (or (not filter-fn)
                         (and filter-fn (funcall filter-fn item)))
                 (push (if transform-fn
                         (funcall transform-fn item)
                         item) result))))
    (nreverse result)))


(defmethod search-for-item ((container iteratable-container-mixin) item &key
                            (test (test container)) (key 'identity))
  (%search-in-container container 'iterate-elements item test key))


(defmethod search-for-element ((container iteratable-container-mixin) item &key
                               (test (test container)) (key 'identity))
  (%search-in-container container 'iterate-elements item test key))


(defmethod search-for-node ((container iteratable-container-mixin) item &key
                            (test (test container)) (key 'identity))
  (%search-in-container container 'iterate-nodes item test key))


(defun %search-in-container (container iterator item test key)
  (funcall iterator container
           (lambda (x)
             (when (funcall test item (funcall key x))
               (return-from %search-in-container (values x t)))))
  (values nil nil))


(defmethod search-for-match ((container iteratable-container-mixin) 
                             predicate &key (key 'identity))
  (%search-for-match container predicate key))


(defun %search-for-match (container predicate key)
  (iterate-elements container
                    (lambda (element)
                      (when (funcall predicate (funcall key element))
                        (return-from %search-for-match element))))
  nil)


(defmethod search-for-matching-node ((container iteratable-container-mixin) 
                                     predicate &key (key 'identity))
  (iterate-nodes container
                     (lambda (element)
                       (when (funcall predicate (funcall key element))
                         (return-from search-for-matching-node element))))
  nil)


(defmethod search-for-node ((container container-uses-nodes-mixin) 
                            (item t) &key
                            (test (test container)) (key 'identity))
  (iterate-nodes container
                    (lambda (node)
                      (when (funcall test item (funcall key (element node)))
                        (return-from search-for-node (values node t))))))


(defmethod search-for-node ((container container-uses-nodes-mixin)
                            (item container-node-mixin) &key
                            (test (test container)) (key 'identity))
  (iterate-nodes container
                     (lambda (node)
                       (when (funcall test item (funcall key node))
                         (return-from search-for-node (values node t)))))
  (values nil nil))


(defmethod search-for-node* ((container container-uses-nodes-mixin) 
                             (item t) &key
                             (test (test container)) (key 'identity))
  (search-for-node container (make-node-for-container container item)
                   :test test :key key))


(defmethod search-for-node* ((container container-uses-nodes-mixin)
                             (item container-node-mixin) &key
                             (test (test container)) (key 'identity))
  (iterate-nodes container
                     (lambda (element)
                       (when (funcall test (funcall key item) (funcall key element))
                         (return-from search-for-node* (values element t)))))
  (values nil nil))

;;; best-item and its friends, argmax and argmin

(defmethod best-item ((container iteratable-container-mixin) function 
                        &key (key 'identity) (test '>) (filter nil))
  (%best-helper container #'iterate-elements function key test filter))

 
(defmethod best-item ((items list) function &key (key 'identity) (test '>)
                      (filter 'identity))
  (%best-helper items #'iterate-elements function key test filter))


(defmethod best-node (container function 
                                &key (key 'identity) (test '>) (filter nil))
  (%best-helper container #'iterate-nodes function key test filter))


(defmethod best-element (container function 
                                   &key (key 'identity) (test '>) (filter nil))
  (%best-helper container #'iterate-elements function key test filter))


(defun %best-helper (container iterator function key test filter)
  (if (empty-p container)
    (values nil nil nil)
    (let ((max-value nil)
          (result nil)
          (result-found? nil))
      (funcall iterator
               container
               (lambda (item)
                 (when (or (not filter)
                           (funcall filter item))
                   (let ((test-value (funcall function (funcall key item))))
                     (when (or (not result-found?)
                               (funcall test test-value max-value))
                       (setf max-value test-value
                             result item
                             result-found? t))))))
      (values result max-value t))))


(defmethod argmax ((items t) function &rest args &key key filter)
  (declare (ignore key filter))
  (apply #'best-item items function :test #'> args))


(defmethod argmin ((items t) function &rest args &key key filter)
  (declare (ignore key filter))
  (apply #'best-item items function :test #'< args))

#+test
(progn
  (argmax '(0 1 -2 3 -4) #'square)
  (best-item '(0 1 -4 2 -3) #'square :test #'<)
  (best-item '(0 1 -4 2 -3) #'square)
  (best-item '(0 1 -4 2 -3 1 3 -1 -4 1 2 -4 -1 -2 10) #'square)
  (best-item '(0 1 -4 2 -3 1 3 -1 -4 1 2 -4 -1 -2 10) #'square)
  
  ;;; ---------------------------------------------------------------------------
  
  (argmax (make-container 'list-container :initial-contents '(0 1 -2 3 -4))
            (lambda (x) (* x x)) :filter #'oddp)
  (argmax (make-container 'list-container :initial-contents '(0 1 -2 3 -4))
            (lambda (x) (* x x)) :filter #'evenp)
  (argmin (make-container 'list-container :initial-contents '(1 0 -2 3 -4))
            (lambda (x) (* x x))))


(defmethod reduce-container ((container iteratable-container-mixin)
                             function
                             &rest args
                             &key (key 'identity)  
                             (initial-value nil initial-value-supplied-p)
                             (start 0) end)
  (declare (dynamic-extent args)
           (ignore key initial-value initial-value-supplied-p start end))
  (apply #'reduce-elements container function args))


(defmethod reduce-elements ((container iteratable-container-mixin)
                            function
                            &key (key 'identity)  
                            (initial-value nil initial-value-supplied-p)
                            (start 0) end)
  (reduce-internal 
   container #'iterate-elements function key initial-value
   initial-value-supplied-p start end))


(defmethod reduce-nodes ((container iteratable-container-mixin)
                         function
                         &key (key 'identity)  
                         (initial-value nil initial-value-supplied-p)
                         (start 0) end) 
  (reduce-internal 
   container #'iterate-nodes function key initial-value 
   initial-value-supplied-p start end))


#+NotYet
(defmethod reduce-key-value ((container iteratable-container-mixin)
                         function
                         &key (key 'identity)  
                         (initial-value nil initial-value-supplied-p)) 
  (reduce-internal 
   container #'iterate-nodes function key initial-value
   initial-value-supplied-p start end))


(defun reduce-internal (container iterator function key initial-value supplied-p
                                  start end)
  (cond 
   ((empty-p container)
    (if supplied-p
      (funcall function initial-value)
      (funcall function)))
   (t
    (let ((accumulator (when supplied-p 
                         (funcall key initial-value)))
          (first-time t)
          (count 0))
      (funcall iterator container
                        (lambda (elt)
                          (when (and (>= count start)
                                     (or (not end) 
                                         (< count end)))
                            (let ((elt* (funcall key elt)))
                              (cond 
                               (first-time (setf first-time nil)
                                           (if supplied-p
                                             (setf accumulator 
                                                   (funcall function accumulator elt*))
                                             (setf accumulator elt*)))
                               (t (setf accumulator
                                        (funcall function accumulator elt*))))))))
      (values accumulator)))))

#+test
(progn
  (reduce-container (make-container 'list-container :initial-contents '(0 1 -2 3 -4)) '+)
  (reduce-container (make-container 'list-container :initial-contents '(5 0 1 -2 3 -4)) 'max)
  (reduce-container (make-container 'list-container :initial-contents '(5 0 1 -2 3 -4)) 'max
                    :initial-value 7 :key '1+))


(defmethod delete-item-if ((container iteratable-container-mixin) test)
  (iterate-elements container
                    (lambda (item)
                      (when (funcall test item)
                        (delete-item container item))))
  (values container))


(defmethod first-element ((container iteratable-container-mixin))
  (iterate-nodes
   container
   (lambda (item)
     (return-from first-element item))))


(defmethod (setf first-element) (value (container iteratable-container-mixin))
  (declare (ignore value))
  (error "Don't know how to set the first element of ~A" container))


(defmethod delete-item :after ((container container-uses-nodes-mixin)
                               (item i-know-my-node-mixin))
  (setf (my-node item) nil))


;;; Default (and not necessary efficient generic implementations)

(defmethod delete-list ((container non-associative-container-mixin) list)
  (dolist (item list)
    (delete-item container item))
  container)


(defmethod insert-list ((container non-associative-container-mixin) list)
  (dolist (item list)
    (insert-item container item))
  container)


;;?? :start :end :test?
(defmethod insert-sequence ((container ordered-container-mixin) (sequence array))
  (loop for item across sequence do
        (insert-item container item))
  container)


(defmethod insert-sequence ((container ordered-container-mixin) (sequence list))
  (loop for item in sequence do
        (insert-item container item))
  container)


(defmethod insert-sequence ((container ordered-container-mixin) 
                            (sequence iteratable-container-mixin))
  (iterate-elements sequence (lambda (element) (insert-item container element)))
  container)


(defmethod insert-new-item ((container searchable-container-mixin) item
                            &key (test (test container)) (key (key container)))
  (unless (search-for-item container (funcall key item)
                           :test test :key key)
    (insert-item container item))
  (values container))

(defmethod successor ((container container-uses-nodes-mixin) (item t))
  (%operate-after-finding container item #'successor))

(defmethod predecessor ((container container-uses-nodes-mixin) (item t))
  (%operate-after-finding container item #'predecessor))

(defmethod %operate-after-finding ((container container-uses-nodes-mixin)
				   (element t)
				   operation)
  (let ((element (search-for-node container element
				  :key #'element))) 
    (unless element
      (error 'element-not-found-error :container container 
	     :element element))
    (funcall operation container element)))

;;; contents-as-sequence-mixin

(defmethod size ((container contents-as-sequence-mixin))
  (length (contents container)))


(defmethod empty-p ((container contents-as-sequence-mixin))
  (= (size container) 0))


(defmethod sort-elements ((container contents-as-sequence-mixin) sorter
                           &key (key 'identity))
  (setf (slot-value container 'contents) 
        (sort (contents container) sorter :key key)))

;;; contents-as-array-mixin

(defmethod empty! ((container contents-as-array-mixin))
  (setf (contents container)
        (adjust-array 
	 (contents container) 0 
	 :fill-pointer (array-has-fill-pointer-p (contents container))
	 :element-type (array-element-type (contents container)))))

(defmethod search-for-item ((container contents-as-array-mixin) item &key
                            (test (test container)) (key 'identity))
  (loop for container-item across (contents container) do
        (when (funcall test (funcall key container-item) item)
          (return-from search-for-item container-item))))

(defmethod iterate-nodes ((container contents-as-array-mixin) function)
  (let ((array (contents container))
	(index 0))
    (loop while (< index (length array)) do
          (let ((current-size (length array)))
					;?? non-optimal for vectors
            (funcall function (aref array index))
            (when (= current-size (length array))
              (incf index)))))
  container)

(defmethod find-item ((container contents-as-array-mixin) item)
  (find item (contents container)))

(defmethod some-item-p
    ((container iteratable-container-mixin) (predicate function))
  (%some-thing-p container #'iterate-nodes predicate))

(defmethod every-item-p
    ((container iteratable-container-mixin) (predicate function))
  (%every-thing-p container #'iterate-nodes predicate))

(defmethod some-element-p
    ((container iteratable-container-mixin) (predicate function))
  (%some-thing-p container #'iterate-elements predicate))

(defmethod every-element-p
    ((container iteratable-container-mixin) (predicate function))
  (%every-thing-p container #'iterate-elements predicate))

(defun %every-thing-p (container iterator predicate)
  (funcall iterator container
           (lambda (item)
             (unless (funcall predicate item)
               (return-from %every-thing-p nil))))  
  (values t))

(defun %some-thing-p (container iterator predicate)
  (funcall iterator container
           (lambda (item)
             (when (funcall predicate item)
               (return-from %some-thing-p item))))
  (values nil))


;;; contents-as-list-mixin

(defmethod insert-item ((container contents-as-list-mixin) item)
  (push item (slot-value container 'contents))
  (values item))


(defmethod append-item ((container contents-as-list-mixin) item)
  (setf (contents container) (nreverse (contents container)))
  (push item (contents container))
  (setf (contents container) (nreverse (contents container))))


(defmethod append-new-item ((container contents-as-list-mixin) item
                            &key (test 'eq) (key 'identity))
  (unless (member item (contents container) :test test :key key)
    (append-item container item)))


(defmethod empty-p ((container contents-as-list-mixin))
  (null (contents container)))


(defmethod empty! ((container contents-as-list-mixin))
  (setf (slot-value container 'contents) nil)
  (values))


(defmethod search-for-item ((container contents-as-list-mixin) item &key
                            (test (test container)) (key 'identity))
  (find item (contents container) :test test :key key))


;;; contents-as-hashtable-mixin

#+Remove
;; Gary King 2006-05-14: redundent with the definition for uses-contents-mixin
(defmethod initialize-instance :after ((container contents-as-hashtable-mixin) &key
                                       &allow-other-keys)
  (when (or (not (slot-boundp container 'contents))
            (null (contents container))) 
    (setf (contents container) (make-container-for-contents container))))

(defmethod make-container-for-contents ((container contents-as-hashtable-mixin) 
                                        &rest args)
  (declare (ignore args))
  (make-hash-table :test (test container)))

(defmethod size ((container contents-as-hashtable-mixin))
  (hash-table-count (contents container)))

(defmethod empty! ((container contents-as-hashtable-mixin))
  (clrhash (contents container))
  (values))


;;?? same as method for bag/set-container
(defmethod search-for-item ((container contents-as-hashtable-mixin) item &key
                            (test (test container)) (key 'identity))
  (maphash (lambda (k v)
             (declare (ignore v))
             (when (funcall test item (funcall key k))
               (return-from search-for-item (values k t))))
           (contents container)))


(defmethod iterate-nodes ((container contents-as-hashtable-mixin) fn)
  (iterate-key-value container (lambda (k v)
                                 (declare (ignore k))
                                 (funcall fn v))))


(defmethod iterate-keys ((container contents-as-hashtable-mixin) function)
  (maphash (lambda (k v)
             (declare (ignore v))
             (funcall function k))
           (contents container)))


(defmethod collect-key-value ((container key-value-iteratable-container-mixin) 
                              &rest args &key filter transform)
  (declare (dynamic-extent args)
           (ignore filter transform))
  (apply #'%collect-key-value container args))


(defun %collect-key-value (container &key filter transform)
  (let ((result nil))
    (iterate-key-value container
                       (lambda (k v)
                         (when (or (not filter) (funcall filter k v))  
                           (push (if transform
                                   (funcall transform k v)
                                   (cons k v)) result))))
    (nreverse result)))



(defmethod find-item ((container contents-as-hashtable-mixin) item)
  (values (gethash item (contents container))))


;; slow, but works
(defmethod reverse-find ((container contents-as-hashtable-mixin) 
                         value &key (test (test container)))
  (iterate-key-value container
                     (lambda (k v)
                       (when (funcall test value v)
                         (return-from reverse-find k))))
  nil)


(defmethod find-value ((container contents-as-hashtable-mixin) item)
  (multiple-value-bind (value found?)
                       (gethash item (contents container))
    (when found?
      value)))


;;; stable-associative-container

(defmethod item-at! ((object stable-associative-container) value &rest indexes)
  (multiple-value-bind 
    (o found?) 
    (apply #'item-at (slot-value object 'associative-container) indexes)
    (declare (ignore o))
    (unless found?
      (setf (item-at (slot-value object 'numbered-container) 
                     (incf (slot-value object 'counter)))
            (if (length-1-list-p indexes) (first indexes) indexes)))
    (setf (apply #'item-at (slot-value object 'associative-container) indexes) 
          value)))


(defmethod add-default-item ((object stable-associative-container) &rest indexes)
  (when (has-initial-element-p (slot-value object 'associative-container))
    (setf (item-at (slot-value object 'numbered-container) 
                   (incf (slot-value object 'counter)))
          (if (length-1-list-p indexes) (first indexes) indexes))))
           

(defmethod item-at ((object stable-associative-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (multiple-value-bind 
    (o found?) 
    (apply #'item-at (slot-value object 'associative-container) indexes)
    (unless found?
      (apply #'add-default-item object indexes))
    o))


(defmethod size ((container stable-associative-container))
  (size (slot-value container 'numbered-container)))


(defmethod iterate-keys ((container stable-associative-container) fn)
  (iterate-keys (slot-value container 'associative-container) fn))


(defmethod iterate-nodes ((container stable-associative-container) fn)
  (iterate-nodes (slot-value container 'associative-container) fn))


(defmethod iterate-elements-stably ((container stable-associative-container) fn)
  (iterate-key-value-stably container
                            (lambda (k v)
                              (declare (ignore k))
                              (funcall fn v))))


(defmethod iterate-key-value ((container stable-associative-container) fn)
  (iterate-key-value (slot-value container 'associative-container) fn))


(defmethod collect-keys ((container stable-associative-container)
                         &rest args &key filter transform)
  (declare (ignore filter transform)
           (dynamic-extent args))
  (apply #'collect-keys (slot-value container 'associative-container)
         args))


(defmethod iterate-key-value-stably ((container stable-associative-container) fn)
  (loop for i from 1 to (size (slot-value container 'numbered-container))
        for key = (item-at (slot-value container 'numbered-container) i) do 
        (funcall 
         fn key (apply #'item-at (slot-value container 'associative-container) 
                       (ensure-list key)))))


(defmethod collect-key-value-stably ((container stable-associative-container))
  (let ((result nil))
    (iterate-key-value-stably
     container
     (lambda (k v)
       (push (cons k v) result)))
    (nreverse result)))


(defmethod collect-elements-stably ((container iteratable-container-mixin) 
                                    &key filter transform)
  (collector-internal container #'iterate-elements-stably filter transform))


(defmethod empty! ((container stable-associative-container))
  (setf (slot-value container 'counter) 0)
  (empty! (slot-value container 'associative-container))
  (empty! (slot-value container 'numbered-container)))


;;; associative-array

(defmethod item-at! ((container associative-array) value &rest indexes)
  (setf (apply #'item-at
               (array-data container)
               (loop for index in indexes
                     for i = 0 then (1+ i) collect
                     (tuple-index container i index)))
        value))


(defun tuple-index (container dim index)
  (with-slots (dim-container num-container) container
    (multiple-value-bind (result found?) 
                         (item-at (item-at dim-container dim) index)
      (unless found?
        (setf (item-at (item-at dim-container dim) index)
              (setf result (incf (item-at num-container dim)))))
      result)))


(defmethod item-at ((container associative-array) &rest indexes)
  (declare (dynamic-extent indexes))
  (with-slots (dim-container) container
    (apply #'item-at (array-data container)
           (loop for index in indexes 
                 for dim = 0 then (1+ dim) collect
                 (item-at (item-at dim-container dim) index)))))


(defmethod iterate-nodes ((container associative-array) fn)
  (with-slots (row-container col-container) container
    (loop for i from 0 to (1- (size row-container)) do
          (loop for j from 0 to (1- (size col-container)) do
                (funcall fn (item-at (array-data container) i j))))))


(defmethod container-dimension ((container associative-array) dimension)
  (with-slots (dim-container) container
    (size (item-at dim-container dimension))))


(defmethod dimensions ((container associative-array))
  (with-slots (dimensions dim-container) container
    (loop for dim from 0 to (1- dimensions) collect
          (size (item-at dim-container dim)))))


#+Example
(let ((c (make-container 'associative-array :test #'equal :dimensions 2)))
  (loop for row in (list "one" "two" "three") 
        for row-num = 1 then (1+ row-num) do
        (loop for col in (list "A" "B" "C")
              for col-num = 1 then (1+ col-num) do
              (setf (item-at c row col) (* row-num col-num))))
  
  (print (item-at c "two" "B"))
  c)

;;; some high level helpers

(defmethod find-item ((container container-uses-nodes-mixin) (item t))
  (find-item container (make-node-for-container container item)))


(defmethod insert-item ((container container-uses-nodes-mixin) (item t))
  (let ((node (make-node-for-container container item)))
    (values (insert-item container node)
            node)))


;;; Odd (in a non-prejoritive sense) methods

;;?? Gary King 2003-04-06: cute but probably not cuddly
(defun collect-using (map-fn filter &rest args)
  "Collects stuff by applying the map-fn to the arguments. Assumes that
the map-fn signature has the function to be applied as its last argument."
  (declare (dynamic-extent filter args))
  (let ((result nil))
    (apply map-fn (append args
                          (list (lambda (thing &optional value)
                                  ;;?? Gary King 2003-11-14: value is here to handle
                                  ;; associative containers. But it's not used so this is
                                  ;; really a bit ugly
                                  (declare (ignorable value))
                                  (when (or (not filter) (funcall filter thing))
                                    (push thing result))))))
    
    (nreverse result)))


(defun count-using (map-fn filter &rest args)
  "Counts stuff by applying the map-fn to the arguments. Assumes that
the map-fn signature has the function to be applied as its last argument."
  (let ((result 0))
    (apply map-fn (append args 
                          (list (lambda (thing)
                                  (when (or (not filter) (funcall filter thing))
                                    (incf result))))))
    
    (values result)))


(defmethod container-difference (c1 c2 &key (key1 'identity)
                                    (key2 'identity))
  ;; things in 1 not in 2
  (collect-elements c1 :filter (lambda (e)
                                 (let ((real-e (funcall key1 e)))
                                   (not (search-for-item c2 real-e :key key2))))))


(defun associative-container-p (container)
  "Returns true if the container is associative. Tries to work for native Lisp
containers too."
  (typecase container
    (hash-table (values t t))
    (associative-container (values t t))
    (abstract-container (values nil t))
    (t (values nil nil))))


(defmethod add-initial-contents ((object initial-contents-mixin) 
                                 (initial-contents list))
  (add-initial-contents-internal object initial-contents))


(defmethod add-initial-contents ((object initial-contents-mixin) 
                                 (initial-contents iteratable-container-mixin))
  (add-initial-contents-internal object initial-contents))


(defun add-initial-contents-internal (object initial-contents)
  ;; not necessarily the fastest, but should work as a good default
  (iterate-elements initial-contents
                    (lambda (element)
                      (insert-item object element))))


(defmethod add-initial-contents ((object initial-contents-key-value-mixin)
                                 initial-contents)
  ;; not necessarily the fastest, but should work as a good default
  (loop for contents = initial-contents then (rest contents) 
        while contents do
        (setf (item-at object (first contents)) (second contents))
        (setf contents (rest contents))))


(defmethod element-position ((container iteratable-container-mixin) element
                             &key (test 'eq) (key 'identity))
  (let ((position 0))
    (iterate-elements
     container
     (lambda (e)
       (when (funcall test (funcall key e) element)
         (return-from element-position position))
       (incf position)))
    (values nil)))


(defmethod element-position ((container contents-as-sequence-mixin) element
                             &key (test 'eq) (key 'identity))
  (position element (contents container) :test test :key key))


(defmethod samep ((l1 list-container) 
                  (l2 list-container))
  (set-equal (contents l1) (contents l2)))

(defmethod samep ((l1 list) 
                  (l2 list-container))
  (set-equal l1 (contents l2)))

(defmethod samep ((l1 list-container) 
                  (l2 list))
  (set-equal (contents l1) l2))

(defmethod samep ((l1 list) (l2 list))
  (set-equal l1 l2))


(defmethod reverse-container ((container ordered-container-mixin))
  ;; expensive generic method
  (let ((reversed (nreverse (collect-elements container))))
    (empty! container)
    (iterate-elements reversed (lambda (e) (insert-item container e)))
    container))


;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************