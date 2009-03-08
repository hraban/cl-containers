(in-package #:cl-containers-test)

(deftestsuite test-trees (cl-containers-test)
  ())

(addtest (test-trees)
  insert-delete-consistency
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((c (make-instance class)))
      (insert-item c 31)
      (ensure-same (size c) 1 :test '=)
      (delete-item c 31)
      (ensure-same (size c) 0 :test '=)
      )))

(addtest (test-trees)
  searching-and-deleting-keyed
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((b (make-container class
			     :key #'first
			     :test #'equal)))
      (insert-list b '((2) (3) (10) (1) (4)))
      (ensure-same (size b) 5)
      (ensure-same (first (first-item b)) 1)
      (ensure-same (search-for-node b 1 :key #'first) (containers::first-node b))
      (delete-item b '(2))
      (ensure-same (size b) 4)
      (insert-item b '(7))
      (insert-item b '(-2))
      (insert-item b '(12))
      (ensure-same (size b) 7)
      (empty! b)
      (ensure-same (size b) 0)
      )))

(addtest (test-trees)
  find-on-nonexistant-item
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((c (make-instance class)))
      (ensure-null (item-at c 1))
      (ensure-null (find-element c 1))
      (ensure-null (find-item c 1))
      (ensure-null (find-node c 1))
      )))

(addtest (test-trees)
  find-on-nonexistant-item-nonempty

  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let ((c (make-instance class)))
      (insert-list c '(64 83 68 84 97))
      (ensure-null (item-at c 1))
      (ensure-null (find-element c 1))
      (ensure-null (find-item c 1))
      (ensure-null (find-node c 1))
      )))

(addtest (test-trees)
  randomized-testing
  (ensure-cases (class)
      '(binary-search-tree
	red-black-tree)
    (let* ((count 20)
	   (randlist (loop repeat count
			   collect (random 100)))
	   (c (make-container class )))
      (loop for n in randlist
	    do (insert-item c n))
      (ensure-same (size c) count)
      (ensure-same (first-element c) (apply #'min randlist))
      (ensure-same (last-element c) (apply #'max randlist))
      (loop for n in randlist
	    do (ensure (item-at c n))
	    do (ensure (find-item c n)))
      (ensure-same (collect-elements c)
		   (sort (copy-list randlist) #'<)
		   :test #'equal)


      ;;now remove half the elements and make sure it still makes sense.
      (loop repeat (/ count 2)
	    for n = (pop randlist)
	    do (ensure (typep (delete-element c n)
			      'containers::bst-node)))

      (ensure-same (size c) (length randlist))
      (ensure-same (first-element c) (apply #'min randlist))
      (ensure-same (last-element c) (apply #'max randlist))
      (loop for n in randlist
	    do (ensure (item-at c n))
	    do (ensure (find-item c n)))
      (ensure-same (collect-elements c)
		   (sort (copy-list randlist) #'<)
		   :test #'equal)

      ;;remove the rest
      (ensure-null (empty! c))
      (ensure (empty-p c))
      (ensure-same (size c) 0)
      (ensure-null (collect-elements c)))))

#|
(setf c (make-instance 'red-black-tree))
(insert-item c 1)
(delete-item c 2)
(size c)
c
(collect-nodes c)
(collect-elements c)
(delete-item c 1)
(trace delete-item)
(trace delete-node)
(trace)

(delete-element c 1)
(find-element c 1)

(compute-applicable-methods #'size (list c))
(compute-applicable-methods #'collect-nodes (list c))
(compute-applicable-methods #'iterate-nodes (list c #'identity))
(compute-applicable-methods #'delete-item (list c 1))
|#