(in-package #:ccl)
(use-package "LIFT")

(deftestsuite test-queue () 
  ((q (make-instance 'basic-queue))))

(defmethod collect-elements ((container basic-queue))
  (let ((result nil))
    (iterate-container container (lambda (x) (push x result)))
    (nreverse result)))

(deftestsuite test-delete-queue (test-queue) 
  ()
  (:setup (insert-item q :a)
          (insert-item q :b)
          (insert-item q :c)))

(addtest (test-delete-queue)
  delete-1
  (delete-item q :a)
  (ensure-same (collect-elements q) '(:b :c) :test #'equal))

(addtest (test-delete-queue)
  delete-2
  (delete-item q :b)
  (ensure-same (collect-elements q) '(:a :c) :test #'equal))

(addtest (test-delete-queue)
  delete-3
  (delete-item q :c)
  (ensure-same (collect-elements q) '(:a :b) :test #'equal))

(addtest (test-delete-queue)
  delete-1-insert-1
  (delete-item q :a)
  (insert-item q :a)
  (ensure-same (collect-elements q) '(:b :c :a) :test #'equal))

(addtest (test-delete-queue)
  delete-2-insert-2
  (delete-item q :b)
  (insert-item q :b)
  (ensure-same (collect-elements q) '(:a :c :b) :test #'equal))

(addtest (test-delete-queue)
  delete-3-insert-3
  (delete-item q :c)
  (insert-item q :c)
  (ensure-same (collect-elements q) '(:a :b :c) :test #'equal))

#+Ignore
(addtest (test-delete-queue)
  delete-3-insert-3
  (inspect q))


