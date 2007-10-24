
(in-package #:containers)

(lift:deftestsuite test-queues (test-containers) ())

(lift:deftestsuite test-single-queue (test-queues) 
  (q)
  (:cases (q (mapcar #'make-container (concrete-subclass-names-of 'abstract-queue)))))

#+Ignore
(lift:addtest (test-single-queue)
  test-ok
  (spy q))

(lift:addtest (test-single-queue)
  test-add-and-empty
  (insert-item q 2)
  (insert-item q 45)
  (lift:ensure-same (size q) 2)
  (empty! q)
  (lift:ensure (empty-p q)))


#+Test
(concrete-subclass-names-of 'abstract-queue)

#+Test
(mapcar #'make-container
        (concrete-subclass-names-of 'abstract-queue))
