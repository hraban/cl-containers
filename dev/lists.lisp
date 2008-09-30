(in-package #:containers)

;;; List Container

(defmethod insert-list ((container list-container) list)
  (setf (contents container) (append (contents container) list))
  (values list))


(defmethod first-element ((container list-container))
  (first-element (contents container)))


(defmethod (setf first-element) (value (container list-container))
  (setf (first-element (contents container)) value))


(defmethod delete-item ((container list-container) item)
  (setf (slot-value container 'contents)
        (delete item (contents container)
                :count 1
                :test (test container)))
  (values item))


(defmethod find-item ((container list-container) item)
  (warn "find-item for list-containers is obsolete, use the semantically
slower 'search-for-item' instead.")
  (search-for-item container item))


(defmethod delete-first ((container list-container))
  (prog1
    (first (contents container))
    (setf (slot-value container 'contents) (rest (contents container)))))


(defmethod nth-element ((container list-container) (index integer))
  (nth index (contents container)))


(defmethod item-at ((container list-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (elt (contents container) (first indexes)))


(defmethod print-container ((container list-container) &optional 
                            (stream *standard-output*))
  (prin1 (contents container) stream)
  container)


(defmethod last-element ((container list-container))
  (last-element (contents container)))


(defmethod (setf last-element) (value (container list-container))
  (setf (last-element (contents container)) value))


;;; sorted-list-container

(defclass* sorted-list-container (sorted-container-mixin
                                    list-container concrete-container)
  "A list container that keeps its items sorted as needed. This uses 'sort'
so it best for small containers."
  ((dirty? nil r)
   (stable? nil ia))
  (:export-slots stable?)
  (:export-p t))


(defmethod set-dirty-flag ((container sorted-list-container) flag)
  (setf (slot-value container 'dirty?) flag))


(defmethod clean-up ((container sorted-list-container))
  (when (dirty? container)
    (set-dirty-flag container nil)
    (setf (contents container) 
          (if (stable? container)
            (stable-sort (contents container) (sorter container) :key (key container))
            (sort (contents container) (sorter container) :key (key container))))))


(defmethod insert-list ((container sorted-list-container) (list t))
  (set-dirty-flag container t)
  (call-next-method))


(defmethod insert-item ((container sorted-list-container) (item t))
  (set-dirty-flag container t)
  (call-next-method))


(defmethod delete-item ((container sorted-list-container) (item t))
  (set-dirty-flag container t)
  (call-next-method))


(defmethod first-element ((container sorted-list-container))
  (clean-up container)
  (call-next-method))


(defmethod (setf first-element) (value (container sorted-list-container))
  (declare (ignore value))
  (clean-up container)
  (call-next-method))


(defmethod delete-first ((container sorted-list-container)) 
  (clean-up container)
  (call-next-method))


(defmethod item-at ((container sorted-list-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (clean-up container)
  (elt (contents container) (first indexes)))


(defmethod print-container ((container sorted-list-container) &optional 
                            (stream *standard-output*))
  (declare (ignore stream))
  (clean-up container)
  (call-next-method))


(defmethod iterate-nodes ((container sorted-list-container) fn) 
  (declare (ignore fn))
  (clean-up container)
  (call-next-method))


(defmethod collect-elements ((container sorted-list-container)
                             &key filter transform) 
  (declare (ignore filter transform))
  (clean-up container)
  (call-next-method))


(defmethod ensure-sorted ((container sorted-list-container))
  (clean-up container)
  container)

(defmethod force-sort ((container sorted-list-container))
  (setf (slot-value container 'dirty?) t)
  container)


;;; dlist-container :: doubly-linked-list container

(defclass* dlist-container-node (container-node-mixin)
  ((next-item nil ia)
   (previous-item nil ia)
   (element nil))
  (:documentation "A double-linked list node"))

(defmethod print-object ((node dlist-container-node) stream)
  (print-unreadable-object (node stream :type t :identity t)
    (format stream "~A" (element node))))

(defclass* dlist-container (ordered-container-mixin
                              iteratable-container-mixin
                              container-uses-nodes-mixin
                              concrete-container)
  ;;?? nil is not a dlist-container-node so the type clause is bogus
  ((first-element nil ia 
		  #+(or)
		 :type
		 #+(or) 
		 dlist-container-node)
   (last-element nil ia 
		 #+(or)
		 :type
		 #+(or) 
		 dlist-container-node)
   (size 0 ia))
  (:documentation "A double-linked list"))

(defmethod make-node-for-container ((container dlist-container) 
				    (item t) &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'dlist-container-node
         :element item args))

(defmethod empty! ((container dlist-container))
  (setf (slot-value container 'size) 0)
  (setf (slot-value container 'first-element) nil
        (slot-value container 'last-element) nil))

(defmethod insert-item ((list dlist-container) (node dlist-container-node)) 
  (insert-item-after list (last-element list) node))

(defmethod insert-item-after ((list dlist-container) node item)
  (insert-item-after list node (make-node-for-container list item)))

(defmethod insert-item-after ((list dlist-container) node
			      (new-node dlist-container-node))
  (declare (ignore node))
  (setf (first-element list) new-node
        (last-element list) new-node)
  (setf (size list) 1)
  list)

(defmethod insert-item-after ((list dlist-container)
			      (node dlist-container-node) item)
  (insert-item-after list node (make-node-for-container list item)))

(defmethod insert-item-after ((list dlist-container) 
			      (node dlist-container-node) 
                              (new-node dlist-container-node))
  (setf (next-item new-node) (next-item node))
  (setf (previous-item new-node) node)
  (if (eq node (last-element list))
    (setf (last-element list) new-node)
    (setf (previous-item (next-item node)) new-node))
  (setf (next-item node) new-node)
  (incf (size list))
  list)

(defmethod insert-item-before ((list dlist-container) node item)
  (declare (ignore node))
  (insert-item list item))

(defmethod insert-item-before ((list dlist-container)
			       (node dlist-container-node) item)
  (insert-item-before list node (make-node-for-container list item)))

(defmethod insert-item-before ((list dlist-container)
			       (node dlist-container-node) 
                               (new-node dlist-container-node))
  (setf (next-item new-node) node)
  (setf (previous-item new-node) (previous-item node))
  (if (eq node (first-element list))
    (setf (first-element list) new-node)
    (setf (next-item (previous-item node)) new-node))
  (setf (previous-item node) new-node)
  (incf (size list))
  list)

(defmethod delete-item-after ((list dlist-container)
			      (node dlist-container-node))
  (cond
   ((not (equal (last-element list) node))
    (let ((next (next-item node)))
      (if (eq next (last-element list))
        (setf (last-element list) node)
        (setf (previous-item (next-item next)) node))
      (setf (next-item node) (next-item next))
      (decf (size list))
      (values (element next) t)))
   (t (values nil nil))))

(defmethod delete-item-before ((list dlist-container)
			       (node dlist-container-node))
  (cond
   ((not (equal (first-element list) node))
    (let ((previous (previous-item node)))
      (if (eq previous (first-element list))
        (setf (first-element list) node)
        (setf (next-item (previous-item previous)) node))
      (setf (previous-item node) (previous-item previous))
      (decf (size list))
      (values (element previous) t)))
   (t (values nil nil))))

(defmethod delete-item ((list dlist-container) item)
  (iterate-nodes 
   list 
   (lambda (node)
     (when (funcall (test list) (element node) item)
       (cond
	 ((eq node (last-element list))
	  (cond
	    ((eq node (first-element list))
	     (setf (first-element list) nil)
	     (setf (last-element list) nil)
	     (decf (size list))
	     (element node))
	    (t (delete-item-after 
		list 
		(previous-item node)))))
	 (t (delete-item-before 
	     list 
	     (next-item node))))))))

(defmethod delete-item ((list dlist-container) (node dlist-container-node))
  (delete-item list (element node)))

(defmethod iterate-nodes ((list dlist-container) fn)
  (loop repeat (size list)
        with node = (first-element list) do
        (funcall fn node)
        (setf node (next-item node)))
  list)

(defmethod item-at ((list dlist-container) &rest indexes)
  (declare (dynamic-extent indexes))
  (if (>= (first indexes) (size list))
    (error "index out of bounds.")
    (let ((item (first-element list)))
      (loop repeat (first indexes) do
            (setf item (next-item item)))
      item)))

(defmethod replace-item ((list dlist-container)
			 (node dlist-container-node) item 
                         &key (length 1) finish-item)
  (declare (ignore finish-item))
  (replace-item list node (make-container 'dlist-container-node item) 
                :length length))

(defmethod replace-item ((list dlist-container) (node dlist-container-node) 
                         (start-item dlist-container-node)
                         &key (length 1) (finish-item start-item))
  (let ((previous-item (previous-item node))
        (start-to-finish (loop for counter from 0 
                               with node = start-item do
                               (if (eq node finish-item)
                                 (return counter)
                                 (setf node (next-item node))))))
    (setf (previous-item start-item) previous-item)
    (if (not (equal node (first-element list)))
      (setf (next-item previous-item) start-item)
      (setf (first-element list) start-item))
    (loop for i from 1 to length
          with next = node do
          (cond
           ((and (eq next (last-element list)) (/= i length))
            (decf (size list) (- (1- i) start-to-finish))
            (setf (next-item finish-item) (next-item next))
            (setf (last-element list) finish-item)
            (return list))
           ((= i length)
            (decf (size list) (- (1- i) start-to-finish))
            (setf (next-item finish-item) (next-item next))
            (if (eq next (last-element list))
              (setf (last-element list) finish-item)
              (setf (previous-item (next-item next)) finish-item))
            (return list))
           (t (setf next (next-item next)))))))

(defmethod successor ((container dlist-container) (node dlist-container-node))
  (next-item node))

(defmethod predecessor ((container dlist-container) 
                        (node dlist-container-node))
  (previous-item node))

;;; sorted-dlist-container

(defclass* sorted-dlist-container (sorted-container-mixin dlist-container)
  ()
  (:documentation "A persistently sorted double-linked list")
  (:default-initargs
    :size 0)
  (:export-p t))

  
(defmethod iterate-nodes-about-node ((list sorted-list-container)
                                         (node dlist-container-node) 
                                         left-fn right-fn)
  (iterate-left-nodes list node left-fn)
  (iterate-right-nodes list node right-fn))


(defmethod iterate-nodes-about-node ((list sorted-list-container)
                                         (item i-know-my-node-mixin) 
                                         left-fn right-fn)
  (iterate-nodes-about-node list (my-node item) left-fn right-fn))


(defmethod iterate-nodes-about-node ((list sorted-list-container)
                                         (item t) left-fn right-fn)
  (let ((it (search-for-node* list item :test #'eq :key #'element))) 
    (when it
      (iterate-nodes-about-node list it left-fn right-fn))))


(defmethod insert-item ((list sorted-dlist-container)
			(node dlist-container-node))
  (insert-item-ordered list node))


(defmethod insert-item-ordered-about-node ((list sorted-dlist-container) 
                                           (node dlist-container-node)
                                           (new-node dlist-container-node))
  (when (= (size list) 0)
    (setf (first-element list) new-node
          (last-element list) new-node
          (size list) 1)
    (return-from insert-item-ordered-about-node list))
  (with-slots (key sorter test) list
    (let ((new-key (funcall key (element new-node))))
      (if (and (previous-item node) 
               (funcall sorter new-key 
			(funcall key (element (previous-item node)))))
        (iterate-left-nodes 
         list node
         (lambda (n)
           (let ((current-key (funcall key (element n))))
             (declare (dynamic-extent current-key))
             (cond ((funcall test (element new-node) (element n))
                    (return-from insert-item-ordered-about-node list))

                   ((not (funcall sorter new-key current-key))
                    (insert-item-after list n new-node)
                    (return-from insert-item-ordered-about-node list))))))
        (iterate-right-nodes 
         list node
         (lambda (n)
           (let ((current-key (funcall key (element n))))
             (declare (dynamic-extent current-key))
             (cond ((funcall test (element new-node) (element n))
                    (return-from insert-item-ordered-about-node list))
                   
                   ((funcall sorter new-key current-key)
                    (insert-item-before list n new-node)
                    (return-from insert-item-ordered-about-node list)))))))))
  
  ;; If we make it here, then this is the last item of the list...
  (setf (next-item new-node) (next-item (last-element list)) 
        (next-item (last-element list)) new-node
        (previous-item new-node) (last-element list)
        (last-element list) new-node)
  (incf (size list))
  (values list))

(defmethod insert-item-ordered-about-node ((list sorted-dlist-container) 
                                           (node t)
                                           (new-node dlist-container-node))
  (if (= (size list) 0)
    (setf (first-element list) new-node
          (last-element list) new-node
          (size list) 1)
    (error 
     "insert-item-ordered-about-node called with nil node and non-empty container")))

(defmethod insert-item-ordered-about-node ((list sorted-dlist-container) 
                                           (node t)
                                           (new-node t))
  (insert-item-ordered-about-node 
   list node (make-node-for-container list new-node)))

(defmethod insert-item-ordered ((list sorted-dlist-container) 
                                (new-node dlist-container-node))
  (when (= (size list) 0)
    (setf (first-element list) new-node
          (last-element list) new-node
          (size list) 1)
    (return-from insert-item-ordered list))
  (with-slots (key sorter test) list
    (let ((new-key (funcall key (element new-node))))
      (iterate-nodes
       list
       (lambda (n)
         (let* ((current-key (funcall key (element n))))
           (cond ((funcall test new-key current-key)
                  (return-from insert-item-ordered list))
                 
                 ((funcall sorter new-key current-key)
                  (insert-item-before list n new-node) 
                  (return-from insert-item-ordered list))))))))
  
  ;; If we make it here, then this is the last item of the list...
  (setf (next-item new-node) (next-item (last-element list)) 
        (next-item (last-element list)) new-node
        (previous-item new-node) (last-element list)
        (last-element list) new-node)
  (incf (size list))
  (values list))

(defmethod insert-item-ordered ((list sorted-dlist-container) (new-node t))
  (insert-item-ordered list (make-node-for-container list new-node)))

(defmethod delete-item ((list sorted-dlist-container) item)
  (with-slots (key sorter test) list
    (let ((item-key (funcall key item)))
      (iterate-nodes 
       list
       #'(lambda (node)
           (let ((current-key (funcall key (element node))))
             (when (funcall test current-key item-key)
               (cond
                ((eq node (last-element list))
                 (cond
                  ((eq node (first-element list))
                   (setf (first-element list) nil)
                   (setf (last-element list) nil)
                   (decf (size list))
                   (element node))
                  
                  (t (delete-item-after 
                      list 
                      (previous-item node)))))
                
                (t (delete-item-before 
                    list 
                    (next-item node)))))
             
             ;; Quick out if the item isn't actually in the container:
             (when (funcall sorter item-key current-key)
               (return-from delete-item list))))))))

(defmethod delete-item ((list sorted-dlist-container) 
			(item i-know-my-node-mixin))
  (let ((node (my-node item)))
    (declare (dynamic-extent node))
    (cond
     ((eq node (last-element list))
      (cond
       ((eq node (first-element list))
        (setf (first-element list) nil)
        (setf (last-element list) nil)
        (decf (size list))
        (element node))
       
       (t (delete-item-after 
           list 
           (previous-item node)))))
     
     (t (when (null (next-item node))
          (break))
        (delete-item-before 
         list 
         (next-item node))))))


(defmethod force-sort ((list sorted-dlist-container))
  list)

(defmethod ensure-sorted ((list sorted-dlist-container))
  list)


(defmethod left-node-for-item ((list sorted-dlist-container) (item t))
  (with-slots (key sorter test) list
    (let ((new-key (funcall key item)))
      (cond ((= (size list) 0) (values nil))
            
            ((and (= (size list) 1)
                  (not (funcall sorter new-key
                                (funcall key (element (first-element list))))))
             (values (first-element list)))
            
            (t 
             (iterate-nodes
              list
              (lambda (n)
                (let* ((current-key (funcall key (element n))))
                  (declare (dynamic-extent current-key))
                  (when (or (funcall test new-key current-key)
                            (funcall sorter new-key current-key))
                    (return-from left-node-for-item (previous-item n))))))
             (values (last-element list)))))))


(defmethod right-node-for-item ((list sorted-dlist-container) (item t))
  (with-slots (key sorter test) list
    (let ((new-key (funcall key item)))
      (cond ((= (size list) 0) (values nil))
            
            ((and (= (size list) 1)
                  (funcall sorter new-key
                           (funcall key (element (first-element list)))))
             (values (first-element list)))
            
            (t 
             (iterate-nodes
              list
              (lambda (n)
                (let* ((current-key (funcall key (element n))))
                  (declare (dynamic-extent current-key))
                  (when (or (funcall test new-key current-key)
                            (funcall sorter new-key current-key))
                    (return-from right-node-for-item n)))))
             (values nil))))))

(defmethod left-and-right-nodes-for-item ((list sorted-dlist-container) (item t))
  (with-slots (key sorter test) list
    (let ((new-key (funcall key item)))
      (cond ((= (size list) 0) (values nil nil))
            
            ((= (size list) 1)
             (if (funcall sorter new-key
                          (funcall key (element (first-element list))))
               (values nil (first-element list))
               (values (first-element list) nil)))
            
            (t 
             (iterate-nodes
              list
              (lambda (n)
                (let* ((current-key (funcall key (element n))))
                  (declare (dynamic-extent current-key))
                  (when (or (funcall test new-key current-key)
                            (funcall sorter new-key current-key))
                    (return-from left-and-right-nodes-for-item 
                      (values (previous-item n) n))))))
             (values (last-element list) nil))))))


(defmethod iterate-left-nodes ((list sorted-dlist-container)
                               (item dlist-container-node) fn)
  (let ((neighbor (previous-item item)))
    (loop while neighbor do
          (funcall fn neighbor)
          (setf neighbor (previous-item neighbor)))))

(defmethod iterate-left-nodes ((list sorted-dlist-container)
                               (item i-know-my-node-mixin) fn)
  (iterate-left-nodes list (my-node item) fn))

(defmethod iterate-left-nodes ((list sorted-dlist-container) (item t) fn)
  (let ((it (search-for-node* list item :test #'eq :key #'element)))
    (when it
      (iterate-left-nodes list it fn))))

(defmethod iterate-right-nodes ((list sorted-dlist-container)
                                (item dlist-container-node) fn)
  (let ((neighbor (next-item item)))
    (loop while neighbor do
          (funcall fn neighbor)
          (setf neighbor (next-item neighbor)))))

(defmethod iterate-right-nodes ((list sorted-dlist-container) 
                                (item i-know-my-node-mixin) fn)
  (iterate-right-nodes list (my-node item) fn))

(defmethod iterate-right-nodes ((list sorted-dlist-container) (item t) fn)
  (let ((it (search-for-node* list item :test #'eq :key #'element)))
    (when it
      (iterate-right-nodes list it fn))))


(defmethod iterate-left ((list sorted-dlist-container)
                         (item dlist-container-node) fn &optional (inclusive? nil))
  (when inclusive?
    (funcall fn (element item)))
  (let ((neighbor (previous-item item)))
    (declare (dynamic-extent neighbor)) 
    (loop while neighbor do
          (funcall fn (element neighbor))
          (setf neighbor (previous-item neighbor)))))

(defmethod iterate-left ((list sorted-dlist-container)
                         (item i-know-my-node-mixin) fn &optional (inclusive? nil))
  (iterate-left list (my-node item) fn inclusive?))

(defmethod iterate-left ((list sorted-dlist-container) (item t) fn 
                         &optional (inclusive? nil))
  (let ((it (search-for-node* list item :test #'eq :key #'element)))
    (when it
      (iterate-left list it fn inclusive?))))

(defmethod iterate-right ((list sorted-dlist-container)
                          (item dlist-container-node) fn &optional (inclusive? nil))
  (when inclusive?
    (funcall fn (element item)))
  (let ((neighbor (next-item item)))
    (declare (dynamic-extent neighbor))
    (loop while neighbor do
          (funcall fn (element neighbor))
          (setf neighbor (next-item neighbor)))))

(defmethod iterate-right ((list sorted-dlist-container) 
                          (item i-know-my-node-mixin) fn &optional (inclusive? nil))
  (iterate-right list (my-node item) fn inclusive?))

(defmethod iterate-right ((list sorted-dlist-container) (item t) fn 
                          &optional (inclusive? nil))
  (let ((it (search-for-node* list item :test #'eq :key #'element)))
    (when it
      (iterate-right list it fn inclusive?)))) 


(defmethod sort-update-left ((list sorted-dlist-container)
                             (node dlist-container-node))
  (with-slots (sorter key test) list
    (let ((node-key (funcall key (element node)))
          (next-neighbor (previous-item node)))
      (declare (dynamic-extent node-key next-neighbor))
      (loop while 
            (and next-neighbor
                 (funcall sorter node-key 
                          (funcall key (element next-neighbor)))) do
            (setf (next-item next-neighbor) (next-item node)
                  (previous-item node) (previous-item next-neighbor))
            (when (not (eq next-neighbor (first-element list))) 
              (setf (next-item (previous-item next-neighbor)) node))
            (if (not (eq node (last-element list)))
              (setf (previous-item (next-item node)) next-neighbor)
              (setf (last-element list) next-neighbor))
            (setf (next-item node) next-neighbor
                  (previous-item next-neighbor) node)
            (setf next-neighbor (previous-item node)))
      (when (not next-neighbor)
        (setf (first-element list) node)))))

(defmethod sort-update-right ((list sorted-dlist-container)
                              (node dlist-container-node))
  (with-slots (sorter key test) list
    (let ((node-key (funcall key (element node)))
          (next-neighbor (next-item node)))
      (declare (dynamic-extent node-key next-neighbor))
      (loop while 
            (and next-neighbor
                 (not (funcall sorter node-key 
                               (funcall key (element next-neighbor))))) do
            (setf (previous-item next-neighbor) (previous-item node)
                  (next-item node) (next-item next-neighbor))
            (when (not (eq next-neighbor (last-element list)))
              (setf (previous-item (next-item next-neighbor)) node))
            (if (not (eq node (first-element list)))
              (setf (next-item (previous-item next-neighbor)) next-neighbor)
              (setf (first-element list) next-neighbor))
            (setf (previous-item node) next-neighbor
                  (next-item next-neighbor) node)
            (setf next-neighbor (next-item node)))
      (when (not next-neighbor)
        (setf (last-element list) node)))))

;; The idea here is that the key of the item probably has changed, and we
;; need to find the happy place in the list for the item... 
(defmethod update-item ((list sorted-dlist-container) (node dlist-container-node))
  (with-slots (sorter test key) list
    (if (and (previous-item node)
             (funcall sorter 
                      (funcall key (element node))
                      (funcall key (element (previous-item node)))))
      (sort-update-left list node)
      (sort-update-right list node)))
  (values list))

(defmethod update-item ((list sorted-dlist-container) (item i-know-my-node-mixin))
  (update-item list (my-node item)))

(defmethod update-item ((list sorted-dlist-container) item)
  (let ((it (search-for-node* list item :test #'eq :key #'element)))
    (when it
      (update-item list it))))

#+TESTING
(progn
  (defclass* my-test-item ()
    ((value nil iar)))
  
  (defun make-my-test-item (value)
    (make-instance 'my-test-item :value value))
  
  (defmethod print-object ((item my-test-item) stream)
    (format stream "~A" (value item))))

#+TESTING
(deftestsuite test-sorted-dlist-container (containers) 
  ((the-list (make-container 'sorted-dlist-container
                             :sorter #'<
                             :key #'value
                             :test #'=))
   (value-1 (make-my-test-item 1))
   (value-5 (make-my-test-item 5))
   (value-3 (make-my-test-item 3))
   (value-7 (make-my-test-item 7))
   (value--3 (make-my-test-item -3))
   (value-2 (make-my-test-item 2))))

#+NOT-REALLY-A-TEST
(addtest (test-sorted-dlist-container)
  (ensure
   (progn
     (insert-item the-list value-1)
     (insert-item the-list value-5)
     (insert-item the-list value-3)
     (iterate-elements the-list #'print)
     
     (insert-item the-list value-7)
     (insert-item the-list value--3)
     (insert-item the-list value-2)
     (print "--------------------")
     (iterate-elements the-list #'print)
     
     (setf (value value-3) 0)
     (update-item the-list value-3)
     (print "--------------------")
     (iterate-elements the-list #'print)
     
     (delete-item the-list value-7)
     (print "--------------------")
     (iterate-elements the-list #'print)
     
     (delete-item the-list value--3)
     (print "--------------------")
     (iterate-elements the-list #'print)
     
     (delete-item the-list value-2)
     (print "--------------------")
     (iterate-elements the-list #'print)
     t)))

#+RUN-IT
(run-tests :suite 'test-sorted-dlist-container)

#+MY-TEST
(let ((the-list (make-container 'sorted-dlist-container
                                :sorter #'<
                                :key #'value
                                :test #'=))
      (value-1 (make-my-test-item 1))
      (value-5 (make-my-test-item 5))
      (value-3 (make-my-test-item 3))
      (value-7 (make-my-test-item 7))
      (value--3 (make-my-test-item -3))
      (value-2 (make-my-test-item 2)))
  
  (insert-item the-list value-1)
  (insert-item the-list value-5)
  (insert-item the-list value-3)
  (iterate-elements the-list #'print)
  
  (insert-item the-list value-7)
  (insert-item the-list value--3)
  (insert-item the-list value-2)
  (print "--------------------")
  (iterate-elements the-list #'print)
  
  (setf (value value-3) 9)
  (update-item the-list value-3)
  (print "--------------------")
  (iterate-elements the-list #'print)
  
  (delete-item the-list value-7)
  (print "--------------------")
  (iterate-elements the-list #'print)
  
  (delete-item the-list value--3)
  (print "--------------------")
  (iterate-elements the-list #'print)
  
  (delete-item the-list value-2)
  (print "--------------------")
  (iterate-elements the-list #'print))