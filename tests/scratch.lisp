(in-package #:cl-containers-test)

#|
(defvar *heap* (make-instance 'cl-containers:heap-container))
(cl-containers:insert-new-item *heap* 2)
(cl-containers:insert-item *heap* 10)

(defvar *heap* (make-instance 'binary-search-tree))
(cl-containers:insert-new-item *heap* 10)
(cl-containers:insert-item *heap* 10)

(let ((heap-node (make-node-for-container *heap* 10)))
  (compute-applicable-methods 
   #'CL-CONTAINERS:INSERT-ITEM
   (list cl-containers-test::*heap* heap-node)))
|#

