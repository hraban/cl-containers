(in-package #:containers)

(defun merge-elements (container merge-fn initial-fn 
                                 &key (key 'identity) (test 'equal)
                                 (argument 'identity) (return :both)
                                 (filter (constantly t)))
  (%merge-helper
   container #'iterate-elements 
   merge-fn initial-fn key test argument return filter))

(defun merge-nodes (container merge-fn initial-fn 
                              &key (key 'identity) (test 'equal)
                              (argument 'identity) (return :both)
                              (filter (constantly t)))
  (%merge-helper
   container #'iterate-nodes
   merge-fn initial-fn key test argument return filter))


(defun %merge-helper (container iterator merge-fn initial-fn 
                                key test argument return filter)
  (let ((result (make-container 'associative-container :test test)))
    (funcall iterator
             container
             (lambda (element)
               (when (funcall filter element)
                 (let ((key-value (funcall key element))
                       (argument-value (funcall argument element)))
                   (multiple-value-bind (item found?)
                                        (item-at result key-value)
                     (setf (item-at result key-value)
                           (if found? 
                             (funcall merge-fn item argument-value)
                             (funcall initial-fn argument-value))))))))
    (case return
      (:both (collect-key-value result :transform (lambda (k v) (list k v))))
      (:keys (collect-keys result))
      (:values (collect-elements result)))))

(defun element-counts (container &key (test 'equal) (key 'identity)
                                 (sort nil) (sort-on :counts) (return :both)
                                 (weight (constantly 1))
                                 (filter (constantly t)))
  (%container-counts container #'merge-elements 
                     test key sort sort-on return weight filter))

#+Test
(element-counts '((a 1) (b 2) (c 1) (d 1) (e 2))
                 :key #'second :return :counts :sort #'< :sort-on :counts)

(defun node-counts (container &key (test 'equal) (key 'identity) 
                              (sort nil) (sort-on :counts) (return :both)
                              (weight (constantly 1))
                              (filter (constantly t)))
  (%container-counts container #'merge-nodes 
                     test key sort sort-on return weight filter))

(defun %container-counts 
    (container merger test key sort sort-on return weight filter)
  (assert (member return '(:both :keys :counts)))
  (let ((result
         (funcall merger container
                  (lambda (a b) (incf a (funcall weight b)))
                  weight
                  :key key
                  :test test
                  :return (if sort :both return)
                  :filter filter)))
    (setf result
          (if sort
            (ecase sort-on
              (:counts (sort result sort :key #'second))
              (:values (sort result sort :key #'first))) 
            result))
    
    (cond ((or (eq return :both) (not sort))
           result)
          ((eq return :keys)
           (mapcar #'first result))
          ((eq return :counts)
           (mapcar #'second result)))))

#+Ignore
(defun map-window-over-list (list window-size function)
   (loop for i from 0 to (- (length list) window-size)
        as window = (subseq list 0 window-size) 
        then (nconc (rest window) (list (first current)))
        as current = (nthcdr window-size list) then (rest current)
        do
        (funcall function window)))

#+Ignore
(defun map-window-over-sequence (sequence window-size function)
  (let* ((size (length sequence))
         (temp (make-array size 
                           :initial-contents sequence)))
    (loop for index from 0 to (- size window-size) do
          (funcall function 
                   (make-array window-size
                               :displaced-to temp
                               :displaced-index-offset index)))))

#+Ignore
;;?? This suffers from shared structure if you just try to collect
;; the windows? Should we have another version that copies the window
;; before calling the function?
(defun collect-window-over-list (list window-size function)
  (loop for i from 0 to (- (length list) window-size)
        as window = (subseq list 0 window-size) 
        then (nconc (rest window) (list (first current)))
        as current = (nthcdr window-size list) then (rest current)
        collect
        (funcall function window)))

(defun map-window-over-elements
       (container window-size window-step function &key duplicate-ends?)
  "Moves a windows of size `window-size` across the elements of `container`, stepping by `window-step` each time. At each step, it applies function `fn` to the elements in the current window \(as a list\)."
  (map-window-over-elements-helper 
   container 'iterate-elements
   window-size window-step function duplicate-ends?))

(defun map-window-over-nodes
       (container window-size window-step function &key duplicate-ends?)
  "Moves a windows of size `window-size` across the elements of `container`, stepping by `window-step` each time. At each step, it applies function `fn` to the elements in the current window \(as a list\)."
  (map-window-over-elements-helper 
   container 'iterate-nodes window-size window-step function duplicate-ends?))

(defun collect-window-over-elements
       (container window-size window-step 
	&key (transform 'identity) duplicate-ends?)
  "Moves a windows of size `window-size` across the elements of `container`, stepping by `window-step` each time. At each step, it applies function `fn` to the elements in the current window \(as a list\)."
  (let ((result nil))
    (map-window-over-elements-helper 
     container 'iterate-elements window-size window-step 
     (lambda (w)
       (push (funcall transform w) result))
     duplicate-ends?)
    (nreverse result)))

(defun collect-window-over-nodes
       (container window-size window-step 
	&key (transform 'identity) duplicate-ends?)
  "Moves a windows of size `window-size` across the elements of `container`, stepping by `window-step` each time. At each step, it applies function `fn` to the elements in the current window \(as a list\)."
  (let ((result nil))
    (map-window-over-elements-helper 
     container 'iterate-nodes window-size window-step 
     (lambda (w)
       (push (funcall transform w) result))
     duplicate-ends?)
    (nreverse result)))

(defun map-window-over-elements-helper
       (container iterator window-size window-step function duplicate-ends?)
  (let ((window nil)
	(state :initial)
	(count window-size))
    (unless (empty-p container)
      (when duplicate-ends?
	(setf window (make-list window-size 
				:initial-element (nth-element container 0))
	      state :process))
      (block do-it
	(funcall iterator
		 container
		 (lambda (element)
		   (when (eq state :initial)
		     (push element window)
		     (when (zerop (decf count))
		       (setf window (nreverse window))
		       (setf state :process)))
		   (when (eq state :fill)
		     (setf window (nconc (rest window) (list element)))
		     (when (zerop (decf count))
		       (setf state :process)))
		   (when (eq state :process)
		     (setf state :fill 
			   count window-step)
		     (funcall function window)))))
      (when duplicate-ends?
	(let ((final-element (first (last window))))
	  (dotimes (i (1- window-size))
	    (setf window (nconc (rest window) (list final-element)))
	    (when (zerop (decf count))
	      (funcall function window)
	      (setf count window-step)))))))
  (values container))

(defun map-pairs (container fn)
  (let ((size (size container)))
    (dotimes (i size)
      (dotimes (j size)
        (unless (>= i j)
          (funcall fn
                   (nth-element container i) 
                   (nth-element container j)))))))

#+Old
(defun map-pairs (container fn)
  (u:iterate-over-indexes
   (list (size container) (size container))
   (lambda (indexes)
     (let ((index-1 (first indexes))
           (index-2 (second indexes)))
       (unless (>= index-1 index-2)
         (funcall fn
                  (nth-element container index-1) 
                  (nth-element container index-2)))))))

;;??
(defun collect-pairs (elements)
  (let ((result nil))
    (map-pairs
     elements
     (lambda (a b)
       (push (sort (list a b) #'string-lessp) result)))
    result))

(defmethod unique-elements ((container iteratable-container-mixin) &key
                            (key 'identity))
  (%unique-elements container 'merge-elements key))

(defmethod unique-elements ((container list) &key
                            (key 'identity))
  (%unique-elements container 'merge-elements key))

(defmethod unique-nodes ((container iteratable-container-mixin) &key
                         (key 'identity))
  (%unique-elements container 'merge-nodes key))

(defmethod unique-nodes ((container list) &key
                         (key 'identity))
  (%unique-elements container 'merge-nodes key))

(defun %unique-elements (container iterator key)
  (collect-elements
   (funcall iterator 
    container
    (lambda (old new)
      (declare (ignore old))
      (values new))
    (lambda (new)
      (values new))
    :key key
    :return :values)))


#+Test
(progn
  (time
   (remove-duplicates
    (collect-items (interaction-graph (ds :p-4-4000m))
                   :transform (lambda (v) 
                                (aref "BTT"
                                      (position (aref (symbol-name (id (element v))) 0) "BCT"))))))
  
  (time
   (u::unique-nodes
    (interaction-graph (ds :p-4-4000m))
    :key (lambda (v) 
           (aref "BTT"
                 (position (aref (symbol-name (id (element v))) 0) "BCT"))))))
