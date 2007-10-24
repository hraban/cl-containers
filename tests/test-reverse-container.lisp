(in-package #:cl-containers-test)

;; WIP

(deftestsuite test-reverse-container ()
  ())

(addtest (test-reverse-container)
  test-list-container
  (let ((c (make-container 'list-container :initial-contents '(1 2 3 4))))
    (iterate-elements c #'print)))