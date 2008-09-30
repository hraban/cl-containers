(in-package #:containers)

;;; heap-container

(defclass* heap-container (sorted-container-mixin
                           vector-container
                           container-uses-nodes-mixin)
  ()
  (:default-initargs
    :sorter #'>
    :initial-size 0))


(defclass* heap-node (container-node-mixin)
  ((index nil ia)))


(defmethod print-object ((node heap-node) stream)
  (print-unreadable-object (node stream :type t :identity nil)
    (format stream "~A" (element node))))


(defmethod l-child ((node heap-node) (heap heap-container))
  (item-at heap (l-child-index node)))


(defmethod r-child ((node heap-node) (heap heap-container))
  (item-at heap (r-child-index node)))


(defmethod heap-node-parent ((node heap-node) (heap heap-container))
  (item-at heap (node-parent-index node)))


(defmethod l-child-index ((node heap-node))
   (1+ (ash (index node) 1)))


(defmethod r-child-index ((node heap-node))
   (ash (1+ (index node)) 1))


(defmethod node-parent-index ((node heap-node))
  (ash (index node) -1))


(defmethod exchange-heap-nodes ((n1 heap-node) (n2 heap-node) (heap heap-container))
   (item-at! heap n2 (index n1))
   (item-at! heap n1 (index n2))
   (let ((index (index n2)))
     (setf (index n2) (index n1))
     (setf (index n1) index)))


(defmethod make-node-for-container ((heap heap-container) (element t) &key)
  (when element
    (make-instance 'heap-node
      :element element
      :index 0)))

;;; Float the 'node' down the tree so the subtree
;;; rooted at 'node' obeys the heap property

(defmethod heapify ((heap heap-container) (node heap-node))
  (let ((largest (index node)))
    (when (and (< (l-child-index node) (size heap))
               (funcall
                (sorter heap) 
                (funcall (key heap) (element (l-child node heap)) )
                (funcall (key heap) (element node))))
      (setf largest (l-child-index node)))
    (when (and (< (r-child-index node) (size heap))
               (funcall (sorter heap)
                        (funcall (key heap) (element (r-child node heap)))
                        (funcall (key heap) (element (item-at heap largest)))))
      (setf largest (r-child-index node)))
    (when (not (eq largest (index node)))
      (exchange-heap-nodes (item-at heap largest) node heap)
      (heapify heap (item-at heap largest))))
  heap)


(defmethod initialize-instance :after ((heap heap-container) &key)
   (loop for i from (floor (/  (size heap) 2)) downto 1 do
         (heapify heap (item-at heap (1- i)))))


(defmethod biggest-item ((heap heap-container))
  (unless (empty-p heap)
    (element (first-item heap))))


(defmethod delete-biggest-item ((heap heap-container))
  (when (> (size heap) 0)
    (let ((max (first-item heap)))
      (exchange-heap-nodes (first-item heap) (last-item heap) heap)
      (delete-last heap)
      (heapify heap (first-item heap))
      (setf (index max) nil)
      (element max))))


(defmethod delete-item ((heap heap-container) (node heap-node))
  (labels ((sift (node)
             (when (and (/= 0 (index node))
                        (funcall (sorter heap) 
                                 (element node) (element (heap-node-parent node heap))))
               (exchange-heap-nodes node (heap-node-parent node heap) heap)
               (sift node))))
    (let ((last (last-item heap))
          (key (key heap)))
      (exchange-heap-nodes node last heap)
      (setf (index node) nil)
      (delete-last heap)
      (when (not (eq last node))
        (cond
         ((and (/= 0 (index last))
               (funcall (sorter heap) 
                        (funcall key (element last))
                        (funcall key (element (heap-node-parent last heap)))))
          (exchange-heap-nodes last (heap-node-parent last heap) heap)
          (sift last))
         (t (heapify heap last))))))
  (element node))


(defmethod insert-item ((heap heap-container) (node heap-node))
  (let ((sorter (sorter heap))
        (key (key heap)))
    (setf (index node) (size heap))
    (call-next-method heap node)
    (labels ((find-spot (a-node)
               (when (and (> (index a-node) 0)
                          (funcall
                           sorter
                           (funcall key (element node))
                           (funcall key (element (heap-node-parent a-node heap)))))
                 (exchange-heap-nodes a-node (heap-node-parent a-node heap) heap)
                 (find-spot a-node))))
      (find-spot node))
    (values heap node)))

  
;;; k-best-heap-container

(defclass* k-best-heap-container (bounded-container-mixin heap-container)
  ((k :initform 1
      :initarg :k
      :accessor k-best-number))
  (:documentation "Stores the k *best* values where *best* is defined by sorter.  This
means that the item at the top of the heap is the *worst* item. So if you want the best
items to be the largest items, make sure sorter is '<'.")
  (:default-initargs
    :sorter #'>
    :k 1))


(defmethod insert-item :around ((heap k-best-heap-container) item)
  (if (< (size heap) (k-best-number heap))
    (call-next-method)
    (when (funcall (sorter heap) 
                   (funcall (key heap) 
                            (biggest-item heap))
                   (funcall (key heap) 
                            item)) ; item is better
      (delete-biggest-item heap)
      (call-next-method heap item))))

#+Test
(defun k-best-heap-container-test ()
  (let ((heap (make-container 'k-best-heap-container :k 3)))
    (loop for i in '(10 11 12 13 9 8 15 -1) do
          (insert-item heap i)
          (format t "HEAP = ~A~%" (collect-elements heap)))))
#+Test
(k-best-heap-container-test)