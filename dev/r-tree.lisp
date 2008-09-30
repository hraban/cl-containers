(in-package :eksl-utilities)


#|

R-Trees allow dynamic insertion and deletion of multi-dimensional
spatial structures.  The structures are indexed hierarchically in
nodes of some predefined size.  Each node contains a tight minimum
bounding n-dimensional rectangle over its descedants.  Using pruning
techniques, the r-tree can be searched for fast nearest-neighbor queries.

The interface adheres to the eksl containers library.

For Example:

(defparameter *r-tree* (make-container 'r-tree
                                        :max-node-size 5
                                        :dimensions 4))

creates an r-tree with branching factor 5 that expects insertion of 
4-dimensional
items.


(loop for i from 0 to 1000 do
       (insert-item *r-tree* (make-list 4 :initial-element i)))

inserts the points '(i i i i) for i from 0 to 1000 into the r-tree. 
By default,
a list of n numbers is assumed to be a point in n-dimensional space. 
Furthermore, the
minimum bounding rectangle is assumed to be the point itself.  The insert-item
method also accepts an item of type r-tree-item.  This allows one to 
create arbitrary
spatial items that can be bound by an n-dimensional 
minimum-bounding-rectangle.  When using
insert-item with objects of than points, one must explicitly create 
an r-tree-item and
specify its MBR.

(nearest-neighbors *r-tree* '(100 100 100 100) 3)

finds the 3 nearest neighbors of the point (100 100 100)

Sometimes it's important to associate a label with a spatial-object. 
In this case,
the r-tree-labelled-item should be used as follows:

(insert-item *r-tree* (make-container 'r-tree-labelled-item 
spatial-object label
                                       :mbr *the-mbr-for-spatial-object*
                                       :test *the-equality-test*))

In the case of simple points, this can be reduced to:

(insert-item *r-tree* (make-container 'r-tree-labelled-item '(x0 x1 
... xn) the-label))

Nearest neighbor queries on r-trees with labelled items return (point 
label) pairs instead
of just points.


|#

(export '(r-tree-root-node
          r-tree-max-node-size
          r-tree-min-node-size
          r-tree-dimensions
          r-tree-depth
          node-splitting-fn
          nearest-neighbors
          insert-item
          delete-item))

;;; r-tree
(defclass* r-tree (test-container-mixin container-uses-nodes-mixin)
  ((root-node :accessor r-tree-root-node
              :initarg :root-node
              :initform nil
              :type r-tree-node
              :documentation "The root node of the r-tree")
   (max-node-size :accessor r-tree-max-node-size
                  :initarg :max-node-size
                  :type fixnum
                  :documentation "The maximum branching factor of the r-tree")
   (min-node-size :accessor r-tree-min-node-size
                  :initarg :min-node-size
                  :type fixnum
                  :documentation "The minimum branching factor of the r-tree.
                                   Defaults to min(2, floor(max-node-size/2))")
   (node-splitting-fn :accessor node-splitting-fn
                      :initarg :node-splitting-fn
                      :type function
                      :documentation "The function used to split nodes")
   (r-tree-dimensions :accessor r-tree-dimensions
                      :initarg :dimensions
                      :type fixnum)
   (r-tree-size :accessor size
                :initarg r-tree-size
                :initform 0)
   (test #'equal))
  (:documentation "R-Trees are dynamic data structures that allow
quick insertion, deletion and searching of spatial data objects in
multiple dimensions.  max-node-size refers to the number of records a node
can keep before it needs to be split.  min-node-size, likewise, is the minimum
number of records required in a node.")
  :copy-slots)

;;; make-container :: 'r-tree -> ... -> r-tree
(defmethod make-container ((class (eql 'r-tree)) &rest args)
  (apply #'make-r-tree args))

;;; make-r-tree :: fixnum -> fixnum -> r-tree
(defun make-r-tree (&key (max-node-size 5) (dimensions 2)
                         (min-node-size (max (floor (/ max-node-size 2)) 2))
                         (node-splitting-fn #'r-tree-quadratic-split))
  (make-instance 'r-tree
    :max-node-size max-node-size
    :min-node-size min-node-size
    :dimensions dimensions
    :node-splitting-fn node-splitting-fn))

(defmethod print-object ((r-tree r-tree) stream)
  (print-unreadable-object (r-tree stream :type t :identity t)
    (format stream "~A ~A ~A"
            (r-tree-max-node-size r-tree)
            (r-tree-min-node-size r-tree)
            (r-tree-dimensions r-tree))))


(defmethod print-r-tree ((r-tree r-tree) stream)
  (if (r-tree-root-node r-tree)
    (format stream "~A" (r-tree-root-node r-tree))
    (format stream "()")))

;;; r-tree-depth :: r-tree -> fixnum
#+WAIT
(defmethod r-tree-depth ((r-tree r-tree))
  (1- (ceiling (log (size r-tree) (r-tree-min-node-size r-tree)))))

(defmethod r-tree-depth ((r-tree r-tree))
  (aif (r-tree-root-node r-tree)
       (r-tree-node-depth it)
       0))

;;; r-tree-node

;;?? GWK make this a mixin and fix it
(defclass* r-tree-node (vector-container)
  ((parent-node :accessor r-tree-parent-node
                :initform nil
                :initarg :parent-node)
   (parent-record :accessor r-tree-parent-record
                  :initform nil
                  :initarg :parent-record))
  (:documentation "R-Tree nodes contain a number of records")
  :copy-slots)

(defmethod initialize-instance :after ((node r-tree-node) &key records)
  (mapc (curry #'insert-item node) records))

(defgeneric make-r-tree-node-like (node &key)
  (:documentation "An interface for making an r-tree-node like the parameter"))

(defmethod mbr-for-r-tree-node ((node r-tree-node))
  (apply #'add-mbrs (collect-items node :transform #'mbr)))

(defmethod r-tree-internal-node? ((r-tree r-tree-node))
  nil)

(defmethod r-tree-node-depth ((node r-tree-node) &optional (depth 1))
  (r-tree-node-depth (r-tree-next-node (first-item node)) (1+ depth)))


;;; r-tree-root-node? :: r-tree-node -> boolean
(defmethod r-tree-root-node? ((node r-tree-node))
  (null (r-tree-parent-node node)))

;;; r-tree-leaf-node
(defclass* r-tree-leaf-node (r-tree-node)
  ()
  (:documentation "r-tree-leaf-nodes contain only r-tree-items"))

(defmethod make-container ((class (eql 'r-tree-leaf-node)) &rest args)
  (apply #'make-r-tree-leaf-node args))

(defun make-r-tree-leaf-node (&key (records nil) (parent-node nil) 
                                   (parent-record nil))
  (make-instance 'r-tree-leaf-node
    :records records
    :parent-node parent-node
    :parent-record parent-record))

(defmethod make-r-tree-node-like ((node r-tree-leaf-node)
                                  &key (parent-node nil) (parent-record nil))
  (make-container 'r-tree-leaf-node
                  :parent-node parent-node
                  :parent-record parent-record))

(defmethod print-object ((node r-tree-leaf-node) stream)
  (format stream "(~{ ~A~} )~%" (collect-elements node)))

(defmethod r-tree-node-depth ((node r-tree-leaf-node) &optional (depth 1))
  depth)

;;; r-tree-internal-node
(defclass* r-tree-internal-node (r-tree-node)
  ()
  (:documentation "An internal node"))

(defmethod make-container ((class (eql 'r-tree-internal-node)) &rest args)
  (apply #'make-r-tree-internal-node args))

(defun make-r-tree-internal-node (&key (records nil) (parent-node 
                                                      nil) (parent-record nil))
  (make-instance 'r-tree-internal-node
    :records records
    :parent-node parent-node
    :parent-record parent-record))

(defmethod make-r-tree-node-like ((node r-tree-internal-node)
                                  &key (parent-node nil) (parent-record nil))
  (make-container 'r-tree-internal-node
                  :parent-node parent-node
                  :parent-record parent-record))

(defmethod r-tree-internal-node? ((node r-tree-internal-node))
  t)

(defmethod print-object ((node r-tree-internal-node) stream)
  (format stream "(~{~A~})" (mapcar #'r-tree-next-node 
                                    (collect-elements node))))

;;; mbr
(defclass* mbr (array-container)
  ()
  (:documentation "mbr is a minimum-bounding-rectangle in n-dimensions.  The
data structure is an array-container with two rows, the first rows is the
minimum bound; the second is the maximum bound."))

(defmethod make-container ((class (eql 'mbr)) &rest args)
  (apply #'make-mbr args))

(defun make-mbr (&key (minimum nil)
                      (maximum nil)
                      (dimensions (list 2 (length minimum))))
  (make-instance 'mbr
    :minimum minimum
    :maximum maximum
    :dimensions dimensions))

(defmethod initialize-instance :after ((mbr mbr)
                                       &key (minimum nil) (maximum nil))
  (loop for i from 0 to (1- (/ (size mbr) 2))
        for min in minimum
        for max in maximum do
        (item-at! mbr min 0 i)
        (item-at! mbr max 1 i)))

(defmethod min-point ((mbr mbr) (dimension number))
  (item-at mbr 0 dimension))

(defmethod max-point ((mbr mbr) (dimension number))
  (item-at mbr 1 dimension))

;;; iterate-mbrs :: mbr -> mbr -> fn -> t
(defmethod iterate-mbrs ((mbr1 mbr) (mbr2 mbr) fn)
  (loop for i from 0 to (1- (/ (size mbr1) 2)) do
        (funcall fn
                 (item-at mbr1 0 i) (item-at mbr1 1 i)
                 (item-at mbr2 0 i) (item-at mbr2 1 i))))

;;; mbr-dimension-increase :: mbr -> mbr -> fixnum
;;; this method measures the total amount in dimension 'mbr1' needs to be
;;; increased to enclose 'mbr2' completely.
(defmethod mbr-size-increase ((mbr1 mbr) (mbr2 mbr))
  (loop for i from 0 to (1- (/ (size mbr1) 2)) sum
        (+ (max 0 (- (item-at mbr1 0 i) (item-at mbr2 0 i)))
           (max 0 (- (item-at mbr2 1 i) (item-at mbr1 1 i))))))

;;; mbr-area-increase :: mbr -> mbr -> fixnum
;;; reports the total area mbr1 would be increased so that it completely
;;; encloses mbr2
(defmethod mbr-area-increase ((mbr1 mbr) (mbr2 mbr))
  (- (mbr-area (add-mbrs mbr1 mbr2)) (mbr-area mbr1)))

;;; mbr-area :: mbr -> fixnum
(defmethod mbr-area ((mbr mbr))
  (loop for i from 0 to (1- (/ (size mbr) 2))
        with total-area = 1 do
        (setf total-area (* total-area (- (item-at mbr 1 i) (item-at mbr 0 i))))
        finally (return total-area)))

;;; add-mbr :: mbr -> mbr -> mbr-mixin
(defmethod add-mbrs ((mbr mbr) &rest mbrs)
  (let ((result (make-container 'mbr :dimensions (list 2 (/ (size mbr) 2))))
        (mbrs (push mbr mbrs)))
    (loop for i from 0 to (1- (/ (size mbr) 2)) do
          (item-at! result (reduce #'min
                                   (mapcar (curry-after #'item-at 0 
                                                        i) mbrs)) 0 i)
          (item-at! result (reduce #'max
                                   (mapcar (curry-after #'item-at 1 
                                                        i) mbrs)) 1 i))
    result))

;;; overlap-mbr? :: mbr -> mbr -> boolean
;;; if mbr1 overlaps mbr2, then return true, otherwise return false
;;; overlap means that mbr1 completely contains mbr2
(defmethod overlap-mbr? ((mbr1 mbr) (mbr2 mbr))
  (iterate-mbrs mbr1 mbr2 #'(lambda (min1 max1 min2 max2)
                              (when (or (< min2 min1) (> max2 max1))
                                (return-from overlap-mbr? nil))))
  t)


;;; record-mixin
(defclass* record-mixin (copyable-mixin)
  ((mbr :accessor mbr
        :initarg :mbr
        :type mbr))
  (:documentation "r-tree records and r-tree-items are both records, so
we need to abstract out their equivalence.")
  :copy-slots)

;;; r-tree-record
(defclass* r-tree-record (record-mixin container-node-mixin)
  ((next :accessor r-tree-next-node
         :initarg :next))
  (:documentation "An r-tree record contains a pointer to the nodes below it
and the minimum-bounding-rectangle of those nodes.")
  :copy-slots)

(defmethod make-container ((class (eql 'r-tree-record)) &rest args)
  (apply #'make-r-tree-record args))

(defun make-r-tree-record (&rest args)
  (apply #'make-instance 'r-tree-record args))

(defmethod insert-item :after ((node r-tree-node) (item r-tree-record))
  (setf (r-tree-parent-node (r-tree-next-node item)) node)
  (setf (r-tree-parent-record (r-tree-next-node item)) item))


;;; label-mixin
(defclass* label-mixin (copyable-mixin)
  ((label :accessor label
          :initarg :label))
  (:documentation "provides a label slot")
  :copy-slots)

;;; r-tree-item
(defclass* r-tree-item (record-mixin)
  ((spatial-object :accessor spatial-object
                   :initarg :spatial-object
                   :type t)
   (test :accessor r-tree-item-test
         :initarg :test
         :type function))
  (:documentation "An r-tree item is a spatial-object with and its
minimum-bounding-rectangle.")
  :copy-slots)

(defmethod make-container ((class (eql 'r-tree-item)) &rest args)
  (apply #'make-r-tree-item args))


(defmethod initialize-instance :after ((item r-tree-item) &key)
  (unless (and (slot-boundp item 'mbr)
               (mbr item))
    (setf (mbr item) (make-container 'mbr
                                     :minimum (spatial-object item)
                                     :maximum (spatial-object item)))))

(defun make-r-tree-item (spatial-object &key (mbr nil) (test #'equal))
  (make-instance 'r-tree-item
    :spatial-object spatial-object
    :test test
    :mbr mbr))

(defmethod print-object ((item r-tree-item) stream)
  (print-unreadable-object (item stream :type t :identity t)
    (format stream "~A" (spatial-object item))))


;;; r-tree-labelled-item
(defclass* r-tree-labelled-item (r-tree-item label-mixin)
  ()
  (:documentation "When using r-trees for nearest-neighbor classification it is
often necessary to label each spatial object.  This item provides a slot for
such a label.")
  :copy-slots)

(defmethod make-container ((class (eql 'r-tree-labelled-item)) &rest args)
  (apply #'make-r-tree-labelled-item args))

(defun make-r-tree-labelled-item (spatial-object label &key (mbr nil) 
                                                 (test #'equal))
  (make-instance 'r-tree-labelled-item
    :spatial-object spatial-object
    :label label
    :test test
    :mbr mbr))

;;; insert-item :: r-tree -> (fixnum) -> r-tree
(defmethod insert-item ((r-tree r-tree) (item list))
  (insert-item r-tree (make-container 'r-tree-item item)))

;;; insert-item :: r-tree -> record record-mixin -> r-tree
;;; insert-item* inserts the record in the r-tree, but we also need to record
;;; the the size increase, hence the incf
(defmethod insert-item ((r-tree r-tree) (record record-mixin))
  (incf (size r-tree))
  (insert-item* r-tree record))

;;; insert-item* :: r-tree -> record-mixin -> r-tree
(defmethod insert-item* ((r-tree r-tree) (record record-mixin))
  (let ((node (choose-r-tree-node r-tree record)))
    (insert-item node record)
    (multiple-value-call #'adjust-r-tree r-tree (split-r-tree-node 
                                                 r-tree node)))
  r-tree)

;;; delete-item :: r-tree -> (t) -> r-tree
(defmethod delete-item ((r-tree r-tree) (item list))
  (delete-item r-tree (make-container 'r-tree-item item)))

;;; delete-item :: r-tree -> r-tree-item -> r-tree
(defmethod delete-item ((r-tree r-tree) (item r-tree-item))
  (multiple-value-bind (node record) (find-leaf-node r-tree item)
    (when node
      (delete-item node record)
      (decf (size r-tree))
      (condense-r-tree r-tree node)
      (when (and (= (size (r-tree-root-node r-tree)) 1)
                 (r-tree-internal-node? (r-tree-root-node r-tree)))
        (setf (r-tree-root-node r-tree)
              (r-tree-next-node (first-item (r-tree-root-node r-tree))))
        (setf (r-tree-parent-node (r-tree-root-node r-tree)) nil)
        (setf (r-tree-parent-record (r-tree-root-node r-tree)) nil))))
  r-tree)


;;; find-leaf-node :: r-tree -> r-tree-item -> r-tree-leaf-node
(defmethod find-leaf-node ((r-tree r-tree) (item r-tree-item))
  (awhen (r-tree-root-node r-tree)
    (find-leaf-node it item)))

;;; find-leaf-node :: r-tree-internal-node -> r-tree-item -> r-tree-leaf-node
(defmethod find-leaf-node ((node r-tree-internal-node) (item r-tree-item)) 
  (iterate-container node 
                     #'(lambda (record) 
                         (when (overlap-mbr? (mbr record) (mbr item)) 
                           (multiple-value-bind 
                             (n r) (find-leaf-node (r-tree-next-node record) item) 
                             (when n 
                               (return-from find-leaf-node (values n r))))))) 
  nil) 

#+OLD
(defmethod find-leaf-node ((node r-tree-internal-node) (item r-tree-item))
  (iterate-container node
                     #'(lambda (record)
                         (when (overlap-mbr? (mbr record) (mbr item))
                           (return-from find-leaf-node
                             (find-leaf-node (r-tree-next-node 
                                              record) item)))))
  nil)



;;; find-leaf-node :: r-tree-leaf-node -> r-tree-item -> r-tree-leaf-node
(defmethod find-leaf-node ((node r-tree-leaf-node) (item r-tree-item))
  (iterate-container node #'(lambda (record)
                              (when (and (overlap-mbr? (mbr record) (mbr item))
                                         (funcall (r-tree-item-test item)
                                                  (spatial-object record)
                                                  (spatial-object item)))
                                (return-from find-leaf-node (values 
                                                             node record)))))
  nil)

;;; condense-r-tree :: r-tree -> r-tree-node -> (t) ->
(defmethod condense-r-tree ((r-tree r-tree) (node r-tree-node) 
                            &optional (records nil))
  (if (r-tree-root-node? node)
    (progn
      (format t "~%CONDENSE-R-TREE:  RECORDS = ~{~A~%~}~%" records)
      (mapc (curry #'insert-item* r-tree) records))
    (let ((parent (r-tree-parent-node node))
          (record (r-tree-parent-record node)))
      (cond
       ((< (size node) (r-tree-min-node-size r-tree))
        (delete-item parent record)
        (if (r-tree-internal-node? node)
          (setf records (nconc (collect-elements node) records))
          (setf records (nconc records (collect-elements node)))))
       (t (setf (mbr record) (mbr-for-r-tree-node node))))
      (condense-r-tree r-tree parent records))))



;;; choose-r-tree-node :: r-tree -> r-tree-item -> r-tree-leaf-node
;;; Invoked from the top of the tree, calls choose-leaf with the root node
(defmethod choose-r-tree-node ((r-tree r-tree) (item r-tree-item))
  (aif (r-tree-root-node r-tree)
       (choose-r-tree-node it item)
       (setf (r-tree-root-node r-tree)
             (make-container 'r-tree-leaf-node))))

;;; choose-r-tree-node :: r-tree -> r-tree-record -> r-tree-internal-node
;;; This finds
(defmethod choose-r-tree-node ((r-tree r-tree) (record r-tree-record))
  (let ((tree-depth (r-tree-depth r-tree)))
    (labels ((choose-r-tree-node* (node record current-depth record-depth)
               (if (= (- tree-depth current-depth) record-depth)
                 node
                 (choose-r-tree-node* (r-tree-next-node 
                                       (choose-best-fit node record))
                                      record
                                      (1+ current-depth)
                                      record-depth))))
      (choose-r-tree-node* (r-tree-root-node r-tree)
                           record
                           1
                           (r-tree-node-depth (r-tree-next-node record))))))

;;; choose-r-tree-node :: r-tree-leaf-node -> r-tree-item -> r-tree-leaf-node
(defmethod choose-r-tree-node ((node r-tree-leaf-node) (item r-tree-item))
  (declare (ignore item))
  node)

;;; choose-r-tree-node :: r-tree-internal-node -> r-tree-item -> r-tree-leaf-node
(defmethod choose-r-tree-node ((node r-tree-internal-node) (item r-tree-item))
  (choose-r-tree-node (r-tree-next-node (choose-best-fit node item)) item))


;;; choose-best-fit :: r-tree-internal-node -> record-mixin -> r-tree-node
;;; choose-best-fit finds the node record that requires the least expansion
;;; of it's minimum-bounding-rectangle to insert the record.  When ties
;;; occur, the record with the least area is favored.  If the areas are
;;; identical, then choose arbitrarily.
(defmethod choose-best-fit ((node r-tree-internal-node) (record record-mixin))
  (loop for i from 1 to (1- (size node))
        with best-fit = (first-item node)
        with best-fit-area = (mbr-area (mbr best-fit))
        with best-fit-increase = (mbr-area-increase (mbr best-fit) 
                                                    (mbr record)) do
        (awhen (item-at node i)
          (let ((increase (mbr-area-increase (mbr it) (mbr record)))
                (area (mbr-area (mbr it))))
            (when (or (< increase best-fit-increase)
                      (and (= increase best-fit-increase)
                           (> best-fit-area area)))
              (setf best-fit-increase increase)
              (setf best-fit it)
              (setf best-fit-area area))))
        finally (return best-fit)))

;;; split-r-tree-node :: r-tree -> r-tree-node -> r-tree-node r-tree-node
(defmethod split-r-tree-node ((r-tree r-tree) (node r-tree-node))
  (if (> (size node) (r-tree-max-node-size r-tree))
    (funcall (node-splitting-fn r-tree) r-tree node)
    node))

;;; r-tree-quadratic-split :: r-tree -> r-tree-node -> r-tree-node r-tree-node
(defmethod r-tree-quadratic-split ((r-tree r-tree) (node r-tree-node))
  (let ((left (make-r-tree-node-like node
                                     :parent-node (r-tree-parent-node node)
                                     :parent-record 
                                     (r-tree-parent-record node)))
        (right (make-r-tree-node-like node
                                      :parent-node (r-tree-parent-node node)
                                      :parent-record 
                                      (r-tree-parent-record node))))
    (multiple-value-bind  (rec1 rec2) (pick-node-seeds node)
      (delete-item node rec1)
      (delete-item node rec2)
      (insert-item left rec1)
      (insert-item right rec2))
    (pick-remaining-nodes r-tree node left right)))

;;; pick-node-seeds :: r-tree-node -> r-tree-record r-tree-record
;;; return the pair of records r1 and r2 that maximize:
;;;   area(mbr(r1),mbr(r2)) - area(mbr(r1)) - area(mbr(r2))
(defmethod pick-node-seeds ((node r-tree-node))
  (flet ((area-fn (rec1 rec2)
           (- (mbr-area (add-mbrs (mbr rec1) (mbr rec2)))
              (mbr-area (mbr rec1)) (mbr-area (mbr rec2)))))
    (loop for i from 0 to (1- (size node))
          with seed1 = (first-item node)
          with seed2 = (item-at node 1)
          with area = (area-fn seed1 seed2) do
          (loop for j from 1 to (1- (size node))
                with record1 = (item-at node i) do
                (let* ((record2 (item-at node j))
                       (val (area-fn record1 record2)))
                  (when (> val area)
                    (setf area val)
                    (setf seed1 record1)
                    (setf seed2 record2))))
          finally (return (values seed1 seed2)))))

;;; pick-and-insert-next-node :: r-tree-node -> r-tree-node -> r-tree-node
(defmethod pick-and-insert-next-node ((node r-tree-node) (left r-tree-node)
                                      (right r-tree-node))
  (loop for i from 1 to (1- (size node))
        with mbr-left = (mbr-for-r-tree-node left)
        with mbr-right = (mbr-for-r-tree-node right)
        with record = (first-item node)
        with left-increase = (mbr-area-increase mbr-left (mbr record))
        with right-increase = (mbr-area-increase mbr-right (mbr record))
        with max-diff = (abs (- left-increase right-increase)) do
        (let* ((nextrecord (item-at node i))
               (left-increase* (mbr-area-increase mbr-left (mbr nextrecord)))
               (right-increase* (mbr-area-increase mbr-right (mbr nextrecord)))
               (val (abs (- left-increase right-increase))))
          (when (> val max-diff)
            (setf max-diff val)
            (setf left-increase left-increase*)
            (setf right-increase right-increase*)
            (setf record nextrecord)))
        finally (progn
                  (delete-item node record)
                  (if (< left-increase right-increase)
                    (insert-item left record)
                    (insert-item right record)))))


;;; pick-remaining-node :: r-tree -> r-tree-node -> r-tree-node ->
;;;                        r-tree-node -> r-tree-node r-tree-node
(defmethod pick-remaining-nodes ((r-tree r-tree) (node r-tree-node)
                                 (left r-tree-node) (right r-tree-node))
  (cond
   ((finished-quadratic-split? r-tree node left right) (values left right))
   (t (pick-and-insert-next-node node left right)
      (pick-remaining-nodes r-tree node left right))))


;;; finished-quadratic-split? :: r-tree -> r-tree-node -> r-tree-node ->
;;;                              r-tree-node -> boolean
(defmethod finished-quadratic-split? ((r-tree r-tree) (node r-tree-node)
                                      (left r-tree-node) (right r-tree-node))
  (cond
   ((empty-p node) t)
   ((= (+ (size node) (size left)) (r-tree-min-node-size r-tree))
    (iterate-container node (curry #'insert-item left))
    (empty! node)
    t)
   ((= (+ (size node) (size right)) (r-tree-min-node-size r-tree))
    (iterate-container node (curry #'insert-item right))
    (empty! node)
    t)
   (t nil)))

;;; adjust-r-tree :: r-tree -> r-tree-node -> r-tree-node -> r-tree
(defmethod adjust-r-tree ((r-tree r-tree) (left r-tree-node) &optional right)
  (if (r-tree-root-node? left)
    (if right
      (let* ((lrecord (make-container 'r-tree-record
                                      :next left
                                      :mbr (mbr-for-r-tree-node left)))
             (rrecord (make-container 'r-tree-record
                                      :next right
                                      :mbr (mbr-for-r-tree-node right)))
             (newroot (make-container 'r-tree-internal-node
                                      :records (list lrecord rrecord))))
        (setf (r-tree-parent-node left) newroot)
        (setf (r-tree-parent-record left) lrecord)
        (setf (r-tree-parent-node right) newroot)
        (setf (r-tree-parent-record right) rrecord)
        (setf (r-tree-root-node r-tree) newroot)
        r-tree)
      r-tree)
    (let ((parent (r-tree-parent-node left))
          (record (r-tree-parent-record left)))
      (setf (mbr record) (mbr-for-r-tree-node left))
      (setf (r-tree-next-node record) left)
      (when right
        (insert-item parent (make-container 'r-tree-record
                                            :next right
                                            :mbr (mbr-for-r-tree-node right))))
      (multiple-value-call #'adjust-r-tree r-tree (split-r-tree-node 
                                                   r-tree parent)))))



;;;
;;;                     NEAREST-NEIGHBOR
;;;

(defparameter *nnscount* 0)
(defparameter *promise-pruning* t)


;;; nearest-neighbor-node-mixin
(defclass* nearest-neighbor-node-mixin (copyable-mixin)
  ((neighbor-distance :accessor neighbor-distance
             :initarg :neighbor-distance))
  (:documentation "Base class of nearest-neighbor nodes and promises")
  :copy-slots)

;;; promise 
(defclass* promise (nearest-neighbor-node-mixin)
  ()
  (:documentation "Promises are placed holders in the kbest heap"))

(defun make-promise (distance)
  (make-instance 'promise 
    :neighbor-distance distance))

(defmethod make-container ((class (eql 'promise)) &rest args)
  (apply #'make-promise args))

(defmethod print-object ((n promise) stream)
  (print-unreadable-object (n stream :type t :identity t)
    (format stream "~A" (neighbor-distance n))))

;;; promise-record 

(defclass* promise-record (r-tree-record)
  ((promise :accessor promise
            :initarg :promise))
  (:documentation "Promise records are used when the MinMaxDist pruning
technique is used.  A promised record simply adds a slot to the record
that stores a pointer to a promise.")
  :copy-slots)

(defmethod make-promise-record ((record r-tree-record) promise)
  (make-instance 'promise-record
    :next (r-tree-next-node record)
    :mbr (mbr record)
    :promise promise))

(defmethod make-container ((class (eql 'promise-record)) &rest args)
  (apply #'make-promise-record args))

(defmethod promise-record? ((pr promise-record)) t)

(defmethod promise-record? ((obj t)) nil)

;;; nearest-neighbor-node
(defclass* nearest-neighbor-node (nearest-neighbor-node-mixin)
  ((spatial-object :accessor spatial-object
                   :initarg :spatial-object
                   :initform nil)
   (mbr :accessor mbr
        :initarg :mbr))
  (:documentation "nearest-neighbor-node objects store branch and bound
search information when doing a recursive descent of an r-tree.")
  :copy-slots)

(defmethod make-container ((class (eql 'nearest-neighbor-node)) &rest args)
  (apply #'make-nearest-neighbor-node args))

(defun make-nearest-neighbor-node (&rest args)
  (apply #'make-instance 'nearest-neighbor-node args))

(defmethod r-tree-item->nearest-neighbor-node ((item r-tree-item) distance)
  (make-container 'nearest-neighbor-node
                  :mbr (mbr item)
                  :spatial-object (spatial-object item)
                  :neighbor-distance distance))

(defmethod nearest-neighbor-node-return-value ((node nearest-neighbor-node))
  (spatial-object node))


(defmethod print-object ((n nearest-neighbor-node) stream)
  (print-unreadable-object (n stream :type t :identity t)
    (format stream "~A ~A" (neighbor-distance n) (spatial-object n))))

;;; nearest-neighbor-node
(defclass* labelled-nearest-neighbor-node (nearest-neighbor-node label-mixin)
  ()
  (:documentation "An analagous nearest-neighbor-node class for 
labelled items."))

(defmethod make-container ((class (eql 
                                   'labelled-nearest-neighbor-node)) &rest args)
  (apply #'make-labelled-nearest-neighbor-node args))

(defun make-labelled-nearest-neighbor-node (&rest args)
  (apply #'make-instance 'labelled-nearest-neighbor-node args))

(defmethod r-tree-item->nearest-neighbor-node ((item r-tree-labelled-item) distance)
  (make-container 'labelled-nearest-neighbor-node
                  :mbr (mbr item)
                  :spatial-object (spatial-object item)
                  :label (label item)
                  :neighbor-distance distance))

(defmethod nearest-neighbor-node-return-value ((node 
                                                labelled-nearest-neighbor-node))
  (list (spatial-object node) (label node)))



;;; nearest-neighbor-node-sorter :: nearest-nighbor-node ->
;;;                                 nearest-neighbor-node -> boolean
(defmethod nearest-neighbor-sorter ((nn1 nearest-neighbor-node-mixin)
                                    (nn2 nearest-neighbor-node-mixin))
  (> (neighbor-distance nn1) (neighbor-distance nn2)))

;;; nearest-neighbors :: (t) -> fixnum -> (t)
(defmethod nearest-neighbors ((r-tree r-tree) (item list) k &key (promise-pruning t))
  (nearest-neighbors r-tree (make-container 'r-tree-item item) k
                     :promise-pruning promise-pruning))

;;; nearest-neighbors :: r-tree -> r-tree-item -> fixnum -> (t)
;;; Some explanation on the 'sort' call here:
;;; Since heaps are just vectors, and collect-items iterates through
;;; the vector, we won't get the nearest-neighbor items in their sorted order.
;;; We could write a new collect-items method for heaps, but that would require
;;; not side afffecting the current heap, so we'd have to copy each element
;;; out and put it in a new heap (so the heap-node objects don't change) and
;;; then pop the biggest item off the top.  This seems a bit too much work
;;; for something we don't guarentee of collect-items anyway, so using
;;; sort is a workable and clean solution
(defmethod nearest-neighbors ((r-tree r-tree) (item r-tree-item) k
                              &key (promise-pruning t))
  (setf *promise-pruning* promise-pruning)
  (setf *nnscount* 0)
  (aif (r-tree-root-node r-tree)
       (let ((neighbors (make-container 'heap-container
                                        :sorter #'nearest-neighbor-sorter)))
         (nearest-neighbor-search it item neighbors k)
         (values
           (collect-items neighbors :transform 
                          #'nearest-neighbor-node-return-value)
          *nnscount*))
       (values nil 0)))

#+WAIT
(defmethod nearest-neighbors ((r-tree r-tree) (item r-tree-item) k
                              &key (promise-pruning t))
  (setf *promise-pruning* promise-pruning)
  (setf *nnscount* 0)
  (aif (r-tree-root-node r-tree)
       (let ((neighbors (make-container 'heap-container
                                        :sorter #'nearest-neighbor-sorter)))
         (nearest-neighbor-search it item neighbors k)
         (values
          (sort
           (collect-items neighbors :transform 
                          #'nearest-neighbor-node-return-value)
           #'< 
           :key (curry #'euclidean-distance* (spatial-object item)))
          *nnscount*))
       (values nil 0)))

;;; minimum-distance-metric :: r-tree-item -> mbr -> fixnum
(defmethod minimum-distance-metric ((item r-tree-item) (mbr mbr))
  (loop for i from 0 to (1- (/ (size mbr) 2))
        for p in (spatial-object item) sum
        (let* ((min (min-point mbr i))
               (max (max-point mbr i)))
          (cond
           ((< p min) (euclidean-distance* p min))
           ((> p max) (euclidean-distance* p max))
           (t 0.0)))))

;;; minimum-distance-metric :: r-tree-item -> r-tree-record -> fixnum
(defmethod minimum-distance-metric ((item r-tree-item) (record r-tree-record))
  (minimum-distance-metric item (mbr record)))

;;; min-maximum-distance-metric :: r-tree-item -> r-tree-record -> fixnum
(defmethod min-maximum-distance-metric ((item r-tree-item) (record 
                                                            r-tree-record))
  (min-maximum-distance-metric item (mbr record)))

;;; min-maximum-distance-metric :: r-tree-item -> mbr -> fixnum
(defmethod min-maximum-distance-metric ((item r-tree-item) (mbr mbr))
  (flet ((closest-hyperplane (p min max)
           (if (<= p (/ (+ min max) 2.0)) min max))
         (furthest-hyperplane (p min max)
           (if (>= p (/ (+ min max) 2.0)) min max)))
    (loop for i from 0 to (1- (/ (size mbr) 2))
          for p in (spatial-object item)
          with s = (loop for i from 0 to (1- (/ (size mbr) 2))
                         for p in (spatial-object item) sum
                         (expt (- p (furthest-hyperplane p (min-point mbr i)
                                                         (max-point 
                                                          mbr i))) 2))
          minimize
          (+ (- s (expt (- p (furthest-hyperplane p (min-point mbr i) 
                                                  (max-point mbr i))) 2))
             (expt (- p (closest-hyperplane p (min-point mbr i) 
                                            (max-point mbr i))) 2)))))


;;; euclidean-distance :: r-tree-item -> r-tree-item -> float
(defmethod euclidean-distance ((item r-tree-item) (item2 r-tree-item))
  (sqrt (euclidean-distance* item item2)))

;;; euclidean-distance :: (float) -> (float) -> float
(defmethod euclidean-distance ((pt-0 list) (pt-1 list))
  (sqrt (euclidean-distance* pt-0 pt-1)))

;;; euclidean-distance* :: number -> number -> float
(defmethod euclidean-distance* ((pt-0 number) (pt-1 number))
  (expt (- pt-1 pt-0) 2))

;;; euclidean-distance :: number -> number -> float
(defmethod euclidean-distance ((pt-0 number) (pt-1 number))
  (sqrt (euclidean-distance* pt-0 pt-1)))

;;; euclidean-distance* :: r-tree-item -> r-tree-item -> float
;;; Just like euclidean-distance but without the square root
(defmethod euclidean-distance* ((item r-tree-item) (item2 r-tree-item))
  (euclidean-distance* (spatial-object item) (spatial-object item2)))

;;; euclidean-distance* :: (fixnum) -> (fixnum) -> float
;;; Just like euclidean-distance but without the square root
(defmethod euclidean-distance* ((pt-0 list) (pt-1 list))
  (loop for x in pt-0
        for y in pt-1 sum
        (expt (- x y) 2)))

;;; euclidean-distance* :: r-tree-item -> (fixnum) -> float
(defmethod euclidean-distance* ((item r-tree-item) (item2 list))
  (euclidean-distance* (spatial-object item) item2))


(defmethod euclidean-distance* ((item list) (item2 r-tree-item))
  (euclidean-distance* item (spatial-object item2)))


;;; sort-branch-list :: (r-tree-records) -> r-tree-item -> fn -> (r-tree-records)
(defmethod sort-branch-list ((records list) (item r-tree-item)
                             &key (key (curry #'minimum-distance-metric item)))
  (sort records #'< :key key))


;;; nearest-neighbor-search :: r-tree-leaf-node -> r-tree-item ->
;;;                            heap-container -> number -> nil
;;; When we reach a leaf node, we add the spatial object is it is better than
;;; our worst nearest-neighbor estimates so far, or if we don't have enough
;;; nearest-neighbor estimates yet.
(defmethod nearest-neighbor-search ((node r-tree-leaf-node) (item r-tree-item)
                                    (neighbors heap-container) k)
  (incf *nnscount*)
  (flet ((add-item (item distance)
           (insert-item neighbors (r-tree-item->nearest-neighbor-node 
                                   item distance))))
    (iterate-container node #'(lambda (item2)
                                (let ((distance 
                                       (euclidean-distance* item item2)))
                                  (cond
                                   ((< (size neighbors) k)
                                    (add-item item2 distance))
                                   ((<= distance (neighbor-distance 
                                                  (biggest-item neighbors)))
                                    (delete-biggest-item neighbors)
                                    (add-item item2 distance))
                                   (t nil))))))
  nil)


;;; nearest-neighbor-search :: r-tree-internal-node -> r-tree-item ->
;;;                            heap-container -> number -> nil
(defmethod nearest-neighbor-search ((node r-tree-internal-node) (item r-tree-item)
                                    (neighbors heap-container) k)
  (incf *nnscount*)
  (labels ((helper (records)
             (when records
               (let ((record (first records)))
                 (when (and (promise-record? record)
                            (heap-node-index (promise record)))
                   (delete-item neighbors (promise record)))
                 (when (or (< (size neighbors) k)
                           (< (minimum-distance-metric item record)
                               (neighbor-distance (biggest-item neighbors))))
                   (nearest-neighbor-search (r-tree-next-node record) item neighbors k))
                 (helper (rest records))))))
    (if *promise-pruning*
      (helper (apply-promise-pruning 
               (sort-branch-list (collect-elements node) item) item neighbors k))
      (helper (sort-branch-list (collect-elements node) item)))))

;;; apply-promise-pruning
(defmethod apply-promise-pruning ((records list) (item r-tree-item)
                                  (neighbors heap-container) k)
  (cons (first records)
  (mapcar #'(lambda (record)
              (let ((minmax (min-maximum-distance-metric item record)))
                (cond 
                 ((or (< (size neighbors) k)
                      (< minmax (neighbor-distance (biggest-item neighbors))))
                  (unless (< (size neighbors) k)
                    (delete-biggest-item neighbors))
                  (multiple-value-bind
                    (heap promise) (insert-item neighbors (make-container 'promise minmax))
                    (declare (ignore heap))
                    (make-container 'promise-record record promise)))
                 (t record))))
          (rest records))))

  
#|

OLD STUFF FOR ORIGINAL 1995 ALGORITHM...

;;; nearest-neighbor-search :: r-tree-internal-node -> r-tree-item ->
;;;                            heap-container -> number -> nil
;;; ---------------------------------------------------------------------------\
#+OLD
(defmethod nearest-neighbor-search ((node r-tree-internal-node) (item r-tree-item)
                                    (neighbors heap-container))
  (labels ((helper (records)
             (when records
               (let ((rec (first records))
                     (others (rest records)))
                 (nearest-neighbor-search (r-tree-next-node rec) item 
                                          neighbors k)
                 (helper (prune-up-branch-list others item neighbors k))))))
    (helper (prune-down-branch-list
             (sort-branch-list (collect-elements node) item) item 
             neighbors k))))


;;; prune-down-branch-list :: (r-tree-record) -> r-tree-item ->
;;;                           heap-container -> number -> (r-tree-record)
(defmethod prune-down-branch-list ((records list) (item r-tree-item)
                                   (neighbors heap-container) k)
  (cond
   ((/= k (size neighbors)) records)
   (t (prune-down-branches records item neighbors))))


;;; prune-up-branch-list :: (r-tree-record) -> r-tree-item ->
;;;                         heap-container -> number -> (r-tree-record)
(defmethod prune-up-branch-list ((records list) (item r-tree-item)
                                 (neighbors heap-container) k)
  (cond
   ((/= k (size neighbors)) records)
   (t (prune-up-branches records item neighbors))))

;;; prune-down-branches :: (r-tree-record) -> r-tree-item -> heap-container ->
;;;                        (r-tree-record)
;;; prune records from our list that have min-distance greater than
;;; min-max-distance of some other record, if that min-distance is greater
;;; than the distance of the farthest node
(defmethod prune-down-branches ((records list) (item r-tree-item)
                                (neighbors heap-container))
  (let ((min-minmax (apply #'min (mapcar
                                  (curry #'min-maximum-distance-metric item)
                                  records))))
    (delete-if #'(lambda (record)
                   (let ((min (minimum-distance-metric item record)))
                     (and (> min min-minmax)
                          (> min (neighbor-distance (biggest-item neighbors))))))
               records)))

;;; prune-up-branches :: (r-tree-record) -> r-tree-item -> heap-container ->
;;;                      (r-tree-record)
(defmethod prune-up-branches ((records list) (item r-tree-item)
                              (neighbors heap-container))
  (let ((min-dist (neighbor-distance (biggest-item neighbors))))
    (delete-if-not #'(lambda (record)
                       (< (minimum-distance-metric item record) min-dist))
                   records)))

|#

