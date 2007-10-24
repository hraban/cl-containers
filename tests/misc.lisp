(in-package #:cl-containers-test)

(deftestsuite test-binary-search-tree (cl-containers-test) 
  (b))

(deftestsuite test-bst-simple (test-binary-search-tree)
  ()
  :setup (progn
	   (setf b (make-container 'binary-search-tree
				   :key #'first
				   :test #'equal))
	     (insert-list b '((2) (3) (10) (1) (4)))))

#+(or)
(addtest (test-binary-search-tree)
  (setf b (make-container 'binary-search-tree
                          :key #'first
                          :test #'equal))
  (insert-list b '((2) (3) (10) (1) (4)))
  (ensure-same (size b) 5)
  (ensure-same (first (element (first-item b))) 1)
  (ensure-same (search-for-node b 1 :key #'first) (first-item b))
  (delete-item b '(2))
  (ensure-same (size b) 4)
  (insert-item b '(7))
  (insert-item b '(-2))
  (insert-item b '(12))
  (ensure-same (size b) 7)
  (empty! b)
  (ensure-same (size b) 0))
