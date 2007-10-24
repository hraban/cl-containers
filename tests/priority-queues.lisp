(in-package #:cl-containers-test)

(deftestsuite test-priority-queue (cl-containers-test)
  ((q (make-instance 'priority-queue-on-container
                     :sorter #'string<
                     :test #'string=))))

(deftestsuite test-delete-priority-queue (test-priority-queue)
  ()
  (:equality-test 'string=)
  (:setup (empty! q)
	  (insert-item q "a")
          (insert-item q "b")
          (insert-item q "c")))

(addtest (test-delete-priority-queue)
  delete-first-1
  (ensure-same (first-element q) "a")
  (delete-first q)
  (ensure-same (first-element q) "b")
  (delete-first q)
  (ensure-same (first-element q) "c"))

(addtest (test-delete-priority-queue)
  delete-first-2
  (dequeue q)
  (ensure-same (first-element q) "b")
  (dequeue q)
  (ensure-same (first-element q) "c")
  (dequeue q)
  (ensure (empty-p q)))


