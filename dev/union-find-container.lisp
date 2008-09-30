(in-package #:containers)

;;; Simple union-find data structure

(defclass* union-find-node (parent-node-mixin container-node-mixin)
  ((rank 0 ir)))


(defmethod initialize-instance :after ((object union-find-node) &key) 
  ;; how kinky
  (setf (parent object) object))


(defmethod print-object ((o union-find-node) stream) 
  (print-unreadable-object (o stream :type nil :identity t)
    (format stream "UFN: ~A, ~D" (element o) (rank o))))


(defmethod make-set (item) 
  (make-instance 'union-find-node :element item))


(defmethod graft-nodes (node1 node2)
  (link-nodes (find-set node1) (find-set node2)))


(defmethod find-set (item)
  (if (eq (parent item) item)
    item
    (setf (parent item) (find-set (parent item)))))


(defmethod link-nodes (node1 node2)
  (if (> (rank node1) (rank node2))
    (setf (parent node2) node1)
    (progn (setf (parent node1) node2)
           (when (= (rank node1) (rank node2))
             (incf (slot-value node2 'rank))))))

;;; union find

(defclass* union-find-container (contents-as-hashtable-mixin)
  ())


(defmethod insert-item ((container union-find-container) item)
  (setf (item-at-1 (contents container) item) 
        (make-instance 'union-find-node :element item))
  item)


(defmethod representative ((container union-find-container) item)
  (element (representative-node container item)))


(defmethod representative-node ((container union-find-container) item)
  (find-set (item-at-1 (contents container) item)))


#+Test 
(let ((a (make-set 'a)) (b (make-set 'b))
      (c (make-set 'c)) (d (make-set 'd))
      (e (make-set 'e)) (f (make-set 'f))
      (g (make-set 'g)))
  
  (graft-nodes g c)
  (graft-nodes f e)
  (graft-nodes e d)
  (graft-nodes d c)
  (graft-nodes c b)
  (graft-nodes b a)
  f)

