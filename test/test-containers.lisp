(in-package #:cl-containers-test)

(deftestsuite cl-containers-test () ())

#|
;;; ---------------------------------------------------------------------------
;;; out of range errors
;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-index-out-of-range (cl-containers-test) 
  (container-class container)
  (:cases (container-class '(list-container vector-container)))
  (:setup (setf container (make-container container-class))))

(lift:addtest (test-index-out-of-range)
  (spy container))

(lift:addtest (test-index-out-of-range) 
  (spy container-class)
  (insert-item container 1)
  (insert-item container 2)
  (insert-item container 3)
  (insert-item container 4)
  (lift:ensure-same (nth-element container 0) 1 :test #'=)
  (lift:ensure-same (nth-element container 2) 3 :test #'=)
  #+Ignore
  (lift:ensure-error (nth-element container 10))
  #+Ignore
  (lift:ensure-error (nth-element container -1)))


;;; ---------------------------------------------------------------------------
;;; list-containers
;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-initial-contents ()
  (container-class)
  (:cases (container-class (concrete-subclass-names-of 'initial-contents-mixin)))
  (:timeout 1))

(lift:addtest (test-initial-contents)
  (spy container-class))

(lift:addtest (test-initial-contents)
  (spy container-class)
  (lift:ensure-same (collect-elements 
                     (make-container container-class
                      :initial-contents '(2 3 4)))
                    '(2 3 4)))

;;; ---------------------------------------------------------------------------
;;; test-binary-search-tree
;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-binary-search-tree () 
  (b))

(lift:addtest (test-binary-search-tree)
  (setf b (make-container 'binary-search-tree
                          :key #'first
                          :test #'equal))
  (insert-list b '((2) (3) (10) (1) (4)))
  (lift:ensure-same (size b) 5)
  (lift:ensure-same (first (first-element b)) 1)
  (lift:ensure-same (element (search-for-node b 1 :key #'first))
		    (first-item b))
  (delete-item b '(2))
  (lift:ensure-same (size b) 4)
  (insert-item b '(7))
  (insert-item b '(-2))
  (insert-item b '(12))
  (lift:ensure-same (size b) 7)
  (empty! b)
  (lift:ensure-same (size b) 0))

;;; ---------------------------------------------------------------------------
;;; Utilities
;;; ---------------------------------------------------------------------------

(defun concrete-subclass-names-of (class-name)
  (let ((result nil))
    (mopu:map-subclasses
     class-name
     (lambda (subclass)
       (when (member 'concrete-container (mopu:mopu-class-direct-superclasses subclass)
                     :key #'class-name)
         (push (class-name subclass) result))))
    (nreverse result)))


;;; ---------------------------------------------------------------------------
;;; move eventually
;;; ---------------------------------------------------------------------------



;;; ---------------------------------------------------------------------------
;;; sorted-list-container
;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-sorted-list-container (cl-containers-test) 
  ((c (make-container 'sorted-list-container))))

(lift:addtest (test-sorted-list-container)
  stays-sorted
  (insert-list c '(4 2 3))
  (lift:ensure (eq (first-item c) 2))
  (insert-item c 23)
  (lift:ensure (eq (item-at c 3) 23)))

;;; ---------------------------------------------------------------------------
;;; collect-keys
;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-collect-keys (cl-containers-test)
  ())

(lift:addtest (test-collect-keys)
  associative-container
  (let ((c (make-container 'associative-container)))
    (loop for (k v) in '((a 2) (b 3) (g 23)) do
          (setf (item-at c k) v))
    (lift:ensure-same '(a b g) (collect-keys c) :test u:set-equal))))

;;; ---------------------------------------------------------------------------

(lift:addtest (test-collect-keys)
  alist-container
  (let ((c (make-container 'alist-container)))
    (loop for (k v) in '((a 2) (b 3) (g 23)) do
          (setf (item-at c k) v))
    (lift:ensure-same '(a b g) (collect-keys c) :test u:set-equal)))
  
;;; ---------------------------------------------------------------------------
;;; associative-containers
;;; ---------------------------------------------------------------------------

(lift:deftestsuite test-associative-containers (cl-containers-test) ())

;;; ---------------------------------------------------------------------------

(lift:addtest (test-associative-containers)
  test-alist-string-equal
  (let ((a (make-container 'alist-container :test #'string-equal)))
    (setf (item-at a "Hi") "Bye")
    (setf (item-at a "Hi") "Hello")
    (lift:ensure-same (item-at a "Hi") "Hello" :test string-equal)))

#+Ignore
;; how is the test supposed to be applied in any case?
(lift:addtest (test-associative-containers)
  test-alist-string-equal-2
  (let ((a (make-container 'alist-container :test #'string-equal)))
    (setf (item-at a "Hi" "1") "Bye")
    (setf (item-at a "Hi" "1") "Bye Bye")
    (setf (item-at a "Hi" "2") "Hello")
    (setf (item-at a "Hi" "2") "Hello there")
    (lift:ensure-same (item-at a "Hi" "1") "Bye Bye" :test string-equal)
    (lift:ensure-same (item-at a "Hi" "2") "Hello there" :test string-equal)))

(lift:addtest (test-associative-containers)
  looking-does-not-add
  (let ((c (make-container 'alist-container)))
   (item-at c :hello)
    (item-at c :bye)
   (lift:ensure (null (contents c)))))


|#


(deftestsuite test-priority-queue-on-container (cl-containers-test)
  ())

(addtest (test-priority-queue-on-container)
  add-and-delete
  (let ((c (make-container 'priority-queue-on-container)))
    (insert-item c 2)
    (insert-item c 1)
    (insert-item c 3)
    (delete-element c 2)
    (ensure-same (size c) 2 :test '=)
    (ensure-same (first-element c) 1)))
