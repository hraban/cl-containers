(in-package #:cl-containers-test)

(deftestsuite test-trees (cl-containers-test)
  ())

(addtest (test-trees)
  insert-delete-consistency
  (ensure-cases (class)
      '((binary-search-tree) 
	(red-black-tree))
    (let ((c (make-instance class)))
      (insert-item c 31)
      (ensure-same (size c) 1 :test '=)
      (delete-item c 31)
      (ensure-same (size c) 0 :test '=)
      )))

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