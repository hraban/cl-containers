(in-package #:cl-containers-test)

(deftestsuite test-dlist-container (cl-containers-test)
  ())

(addtest (test-dlist-container
	  :documentation "case 207")
  test-size
  (ensure-same (size (make-container 'dlist-container)) 0))

