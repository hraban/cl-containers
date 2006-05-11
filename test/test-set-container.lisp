(in-package #:u)

(deftestsuite test-set-container () 
  ((c (make-container 'set-container :initial-contents '(1 2 3 3 3 4)))))

(addtest (test-set-container)
  (ensure-same (size c) 6))
    
(addtest (test-set-container)
  (ensure-same (count-elements c 3) 3)
  (ensure-same (count-elements c 1) 1)) 