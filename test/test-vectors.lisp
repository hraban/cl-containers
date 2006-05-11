
(in-package #:containers)

(lift:deftestsuite test-vectors (test-containers) ())

(lift:deftestsuite test-initial-contents-mixin () ())

(lift:deftestsuite test-vector-containers () ())

(lift:addtest (test-vector-containers)
  test-initial-contents
  (lift:ensure-same (collect-elements 
                (make-container 'vector-container
				:initial-contents '(2 3 4)))
		    '(2 3 4)))

 
 