(in-package #:containers)

;;; generic tree classes

;;; tree-container
(defclass* abstract-tree-container (abstract-container) ())

;;; rooted-tree-container ::

(defclass* rooted-tree-container (abstract-tree-container)
  ((root nil ia))
  (:documentation "Base class of all trees with roots."))

(defclass* many-child-node (container-node-mixin 
                              iteratable-container-mixin) 
  ())

(defmethod iterate-children ((node many-child-node) fn)
  (iterate-nodes node fn))

(defmethod has-children-p ((node many-child-node))
  (iterate-children 
   node
   (lambda (v) 
     (declare (ignore v))
     (return-from has-children-p t)))
  (values nil))
                                                      
(defmethod find-child-node ((node many-child-node) child
                            &key (test #'eq) (key #'identity))
  (find child (children node) :test test :key key))


(defclass* many-ordered-child-node (many-child-node 
                                      vector-container-mixin)
  ()
  (:documentation "A node with many ordered children is a vector"))

(defmethod children ((node many-ordered-child-node))
  (collect-elements node))

(defclass* many-unordered-child-node (many-child-node
                                        contents-as-list-mixin)
  ((contents nil ia))
  (:documentation "Children are unordered"))


;;; Binary-Search-Trees

;; we sort keys of the nodes using sorter; we test equality with test.
(defclass* binary-search-tree (container-uses-nodes-mixin
			       initial-contents-mixin
                                 sorted-container-mixin
                                 findable-container-mixin
                                 iteratable-container-mixin
                                 rooted-tree-container
                                 concrete-container)
  ((tree-size 0 iar))
  (:default-initargs
    :key 'identity
    :test 'eq
    :sorter '<))


(defclass* bst-node (two-child-node)
  ((tree nil ia)))


(defmethod make-node-for-container ((tree binary-search-tree) (item t) &key)
  (if item
    (make-instance 'bst-node
      :element item)
    nil))

(defmethod node-empty-p ((node bst-node))
  (null node))

(defmethod node-empty-p ((node (eql nil)))
  t)

(defmethod print-object ((o bst-node) stream)
  (print-unreadable-object (o stream :type t)
    (format stream "~A" (element o))))


;;; Standard container operations for BST's

(defmethod size ((tree binary-search-tree))
  (tree-size tree))

(defmethod size ((node bst-node))
  (let ((count 0))
    (walk-tree node
               #'(lambda (item)
                   (declare (ignore item))
                   (incf count)))
    count))


(defmethod empty-p ((tree binary-search-tree))
  (node-empty-p (root tree)))


(defmethod empty! ((tree binary-search-tree))
  (setf (root tree) (make-node-for-container tree nil))
  (setf (tree-size tree) 0)
  (values))


;;; Search, max, min, etc.

(defmethod find-item ((tree binary-search-tree) (item bst-node))
  (let* ((key (key tree))
         (test (test tree))
         (sorter (sorter tree))
         (key-item (funcall key (element item)))
         (current (root tree))
         (not-found? nil))
    (loop while (and (not (node-empty-p current))
                     (setf not-found? 
                           (not (funcall test
                                         key-item
                                         (funcall key (element current)))))) do
          (if (funcall sorter key-item
                       (funcall key (element current)))
            (setf current (left-child current))
            (setf current (right-child current))))
    (if (and (not (node-empty-p current)) (not not-found?))
      current
      nil)))

(defmethod find-successor-item ((tree binary-search-tree) (item bst-node))
  "Find the item equal to or the next greater than item"
  (with-slots (key test sorter) tree
    (let ((key-item (funcall key (element item))))
      (labels ((node-equal-p (current)
		 (funcall test key-item
			  (funcall key (element current))))
	       (compare-lt (current)
		 (funcall sorter key-item
			  (funcall key (element current))))
	       (final-node (prior)
		 (if (compare-lt prior) prior
		     (let ((s (successor tree prior)))
		       (if (node-empty-p s) nil s))))
	       (find-successor (current prior)
		 (cond ((node-empty-p current) 
			(when prior (final-node prior)))
		       ((node-equal-p current) current)
		       ((compare-lt current)
			(find-successor (left-child current) current))
		       (t (find-successor (right-child current) current)))))
	(find-successor (root tree) nil)))))
			

(defmethod find-node ((tree binary-search-tree) (item t))
  (find-item tree (make-node-for-container tree item)))

(defmethod find-successor-node ((tree binary-search-tree) (item t))
  (find-successor-item tree (make-node-for-container tree item)))

(defmethod first-element ((node bst-node))
  (element (first-node node)))

(defmethod first-node ((node bst-node))
  (let ((current node))
    (loop while (not (node-empty-p (left-child current))) do
          (setf current (left-child current)))
    current))

(defmethod (setf first-element) (value (node bst-node))
  (let ((current node))
    (loop while (not (node-empty-p (left-child current))) do
          (setf current (left-child current)))
    (setf (element current) value)))

(defmethod first-element ((tree binary-search-tree))
  (first-element (root tree)))

(defmethod first-node ((tree binary-search-tree))
  (first-node (root tree)))

(defmethod (setf first-element) (value (tree binary-search-tree))
  (setf (first-element (root tree)) value))

(defmethod last-element ((node bst-node))
  (element (last-node node)))

(defmethod last-node ((node bst-node))
  (let ((current node))
    (loop while (not (node-empty-p (right-child current))) do
          (setf current (right-child current)))
    current))

(defmethod last-node ((tree binary-search-tree))
  (last-node (root tree)))

(defmethod last-element ((tree binary-search-tree))
  (last-element (root tree)))

(defmethod (setf last-element) (value (node bst-node))
  (let ((current node))
    (loop while (not (node-empty-p (right-child current))) do
          (setf current (right-child current)))
    (setf (element current) value)))


(defmethod (setf last-element) (value (tree binary-search-tree))
  (setf (last-element (root tree)) value))


(defmethod successor ((tree binary-search-tree) (node bst-node))
  (if (not (node-empty-p (right-child node)))
    (first-node (right-child node))
    (let ((y (parent node)))
      (loop while (and (not (node-empty-p y))
                       (eq node (right-child y))) do
            (setf node y
                  y (parent y)))
      y)))


(defmethod predecessor ((tree binary-search-tree) (node bst-node))
  (if (not (node-empty-p (left-child node)))
    (last-node (left-child node))
    (let ((y (parent node)))
      (loop while (and (not (node-empty-p y))
                       (eq node (left-child y))) do
            (setf node y
                  y (parent y)))
      y)))

;;; Insertion and deletion

(defmethod insert-item ((tree binary-search-tree) (item bst-node))
  (loop with key = (key tree)
        with y = (make-node-for-container tree nil)
        ;        with test = (test tree)
        with sorter = (sorter tree)
        and x = (root tree)
        and key-item = (funcall key (element item))
        while (not (node-empty-p x))
        do
        (progn
          ;(format t "~A ~A~%" x y)
          (setf y x)
          (if (funcall sorter key-item (funcall key (element x)))
            (setf x (left-child x))
            (setf x (right-child x))))
        finally (progn
                  (setf (parent item) y
                        (tree item) tree)
                  (incf (tree-size tree))
                  (if (node-empty-p y)
                    (setf (root tree) item)
                    (if (funcall sorter key-item (funcall key (element y)))
                      (setf (left-child y) item)
                      (setf (right-child y) item)))))
  tree)

(defmethod delete-node ((tree binary-search-tree) (node bst-node))
  (let* ((y (if (or (node-empty-p (left-child node))
                    (node-empty-p (right-child node)))
              node
              (successor tree node)))
         (x (if (left-child y)
              (left-child y)
              (right-child y))))
    (when x
      (setf (parent x) (parent y)))
    (if (node-empty-p (parent y))
      (setf (root tree) x)
      (if (equal y (left-child (parent y)))
        (setf (left-child (parent y)) x)
        (setf (right-child (parent y)) x)))
    (if (not (equal y node))
      (setf (element node) (element y)))
    y))

(defmethod delete-node :after ((tree binary-search-tree) (node bst-node))
  (decf (tree-size tree)))
  
(defmethod delete-item ((tree binary-search-tree) (node bst-node))
  (delete-node tree node))

(defmethod delete-item ((tree binary-search-tree) (item t))
  (let ((found (find-node tree item)))
    (if found
      (delete-node tree found)
      tree)))

(defmethod delete-item-if (test (tree binary-search-tree))
  "Iterate over the nodes of the tree, deleting them if they match
test."
  ;; As a first implementation, we use an inorder-walk to collect
  ;; matching nodes into a list and then delete them one at a time.
  (let ((to-delete nil))
    (walk-tree-nodes (root tree)
                     #'(lambda (node)
                         (when (funcall test (element node))
                           (push node to-delete)))
                     :inorder)
    (loop for node in to-delete do
          (delete-item tree node))))


(defmethod iterate-nodes ((tree binary-search-tree) fn)
  (inorder-walk-nodes tree fn))

;;; Tree walking

(defmethod inorder-walk ((tree binary-search-tree) walk-fn)
  (walk-tree (root tree) walk-fn :inorder))

(defmethod preorder-walk ((tree binary-search-tree) walk-fn)
  (walk-tree (root tree) walk-fn :preorder))

(defmethod postorder-walk ((tree binary-search-tree) walk-fn)
  (walk-tree (root tree) walk-fn :postorder))


(defmethod inorder-walk-nodes ((tree binary-search-tree) walk-fn)
  (walk-tree-nodes (root tree) walk-fn :inorder))

(defmethod preorder-walk-nodes ((tree binary-search-tree) walk-fn)
  (walk-tree-nodes (root tree) walk-fn :preorder))

(defmethod postorder-walk-nodes ((tree binary-search-tree) walk-fn)
  (walk-tree-nodes (root tree) walk-fn :postorder))


(defmethod walk-tree ((node bst-node) walk-fn &optional (mode :inorder))
  (walk-tree-nodes node
                   (lambda (x)
                     (funcall walk-fn (element x)))
                   mode))

(defmethod walk-tree ((node (eql nil)) walk-fn &optional (mode :inorder))
  "Special case..."
  (declare (ignore mode walk-fn)))

(defmethod walk-tree-nodes ((node bst-node) walk-fn &optional (mode :inorder))
  (when (eq mode :preorder)
    (funcall walk-fn node))
  (walk-tree-nodes (left-child node) walk-fn mode)
  (when (eq mode :inorder)
    (funcall walk-fn node))
  (walk-tree-nodes (right-child node) walk-fn mode)
  (when (eq mode :postorder)
    (funcall walk-fn node)))

(defmethod walk-tree-nodes ((node (eql nil)) walk-fn &optional (mode :inorder))
  "Special case..."
  (declare (ignore walk-fn mode)))


;;; Red-Black Trees
;;;
;;; A Red-black tree is a binary search tree whose nodes have a color
;;; the insert and delete operations preserve certain properties of this
;;; color that ensure that the tree stays balanced and therefore that all
;;; operations are O( lg n )
;;;
;;; Note that we use *rbt-empty-node* (instead of nil) to signal an empty
;;; node. This makes delete-item cleaner but means that you need to be careful
;;; in the rest of the code to use node-empty-p instead of just assuming that
;;; a node will be nil.

(defconstant +rbt-color-black+ 0)
(defconstant +rbt-color-red+ 1)


(defvar *rbt-empty-node* nil)


(defclass* red-black-tree (binary-search-tree)
  ()
  (:default-initargs
    :key #'identity
    :test #'eq
    :sorter #'<
    :root *rbt-empty-node*))


(defmethod initialize-instance :after ((object red-black-tree) &key
                                       (root *rbt-empty-node*))
  (unless (eq root *rbt-empty-node*)
    (setf (parent root) *rbt-empty-node*)))


(defclass* red-black-node (bst-node)
  ((color :initform +rbt-color-black+
          :initarg :rbt-color
          :accessor rbt-color)
   (right-child :initarg :right-child)          ; add initargs
   (left-child :initarg :left-child))
  (:default-initargs
    :right-child *rbt-empty-node*
    :left-child *rbt-empty-node*))


(defmethod node-empty-p ((node red-black-node))
  (eq node *rbt-empty-node*))


(defmethod make-node-for-container ((tree red-black-tree) (item t) &key)
   (if item
     (make-instance 'red-black-node
       :element item
       :tree tree)
     *rbt-empty-node*))


(defmethod print-object ((o red-black-node) stream)
   (format stream "#<RB-NODE COLOR: ~A, ~A>"
           (if (= (rbt-color o) +rbt-color-black+) "B" "R")
           (element o)))


(setf *rbt-empty-node* (make-instance 'red-black-node
                         :right-child nil
                         :left-child nil
                         :element nil))


(defmethod rotate-left ((tree binary-search-tree) (x two-child-node))
   (assert (not (eq (right-child x) *rbt-empty-node*)))

   (let ((y (right-child x)))
     ;; turn y's left subtree into x's right subtree
     (setf (right-child x) (left-child y))

     (when (not (node-empty-p (left-child y)))
       (setf (parent (left-child y)) x))

     ;; Link's x's parent to y
     (setf (parent y) (parent x))

     (if (node-empty-p (parent x))
       (setf (root tree) y)
       (if (eq x (left-child (parent x)))
         (setf (left-child (parent x)) y)
         (setf (right-child (parent x)) y)))

     ;; put x on y's left
     (setf (left-child y) x
           (parent x) y)))


(defmethod rotate-right ((tree binary-search-tree) (x two-child-node))
   (assert (not (eq (left-child x) *rbt-empty-node*)))

   (let ((y (left-child x)))
     ;; turn y's right subtree into x's left subtree
     (setf (left-child x) (right-child y))

     (when (not (node-empty-p (right-child y)))
       (setf (parent (right-child y)) x))

     ;; Link's x's parent to y
     (setf (parent y) (parent x))

     (if (node-empty-p (parent x))
       (setf (root tree) y)
       (if (eq x (right-child (parent x)))
         (setf (right-child (parent x)) y)
         (setf (left-child (parent x)) y)))

     ;; put x on y's left
     (setf (right-child y) x
           (parent x) y)))


(defmethod insert-item :after ((tree red-black-tree) (item bst-node))
  (assert item)
  (setf (rbt-color item) +rbt-color-red+)

  (let ((y nil))
    (loop while (and (not (eq item (root tree)))
		     (= (rbt-color (parent item)) +rbt-color-red+)) do
	 (if (eq (parent item) (left-child (parent (parent item))))
             (progn
               (setf y (right-child (parent (parent item))))
               (if (= (rbt-color y) +rbt-color-red+)
		   (progn
		     (setf (rbt-color (parent item)) +rbt-color-black+
			   (rbt-color y) +rbt-color-black+
			   (rbt-color (parent (parent item))) +rbt-color-red+
			   item (parent (parent item))))
		   ;; ELSE
		   (progn
		     (when (eq item (right-child (parent item)))
		       (setf item (parent item))
		       (rotate-left tree item))
		     (setf (rbt-color (parent item)) +rbt-color-black+
			   (rbt-color (parent (parent item))) +rbt-color-red+)
		     (rotate-right tree (parent (parent item))))))
             ;; ELSE
             (progn
               (setf y (left-child (parent (parent item))))
               (if (= (rbt-color y) +rbt-color-red+)
		   (progn
		     (setf (rbt-color (parent item)) +rbt-color-black+
			   (rbt-color y) +rbt-color-black+
			   (rbt-color (parent (parent item))) +rbt-color-red+
			   item (parent (parent item))))
		   ;; ELSE
		   (progn
		     (when (eq item (left-child (parent item)))
		       (setf item (parent item))
		       (rotate-right tree item))
		     (setf (rbt-color (parent item)) +rbt-color-black+
			   (rbt-color (parent (parent item))) +rbt-color-red+)
		     (rotate-left tree (parent (parent item))))))))

    (setf (rbt-color (root tree)) +rbt-color-black+)))


(defmethod delete-node ((tree red-black-tree) (item red-black-node))
   (let ((y nil) (x nil))
     (if (or (eq (left-child item) *rbt-empty-node*)
             (eq (right-child item) *rbt-empty-node*))
       (setf y item)
       (setf y (successor tree item)))

     (if (eq (left-child y) *rbt-empty-node*)
       (setf x (right-child y))
       (setf x (left-child y)))

     (setf (parent x) (parent y))

     (if (eq (parent y) *rbt-empty-node*)
       (setf (root tree) x)
       (if (eq y (left-child (parent y)))
         (setf (left-child (parent y)) x)
         (setf (right-child (parent y)) x)))

     (when (not (eq y item))
       (setf (element item) (element y)))

     (when (= (rbt-color y) +rbt-color-black+)
       ;(break)
       (rb-delete-fixup tree x))

     y))

(defmethod rb-delete-fixup ((tree red-black-tree) (x red-black-node))
   (let ((w nil))
     (loop while (and (not (eq x (root tree)))
                      (eq (rbt-color x) +rbt-color-black+)) do

;          (format t "~&RBDF: ~A " x)

           (if (eq x (left-child (parent x)))
             (progn
               (setf w (right-child (parent x)))

;              (format t "RC ~A " w)

               (if (= (rbt-color w) +rbt-color-red+)
                 (progn
                   (setf (rbt-color w) +rbt-color-black+
                         (rbt-color (parent x)) +rbt-color-red+)
                   (rotate-left tree (parent x))
                   (setf w (right-child (parent x)))))

               (if (and (eq (rbt-color (left-child w)) +rbt-color-black+)
                          (eq (rbt-color (right-child w)) +rbt-color-black+))
                 (progn
                   (setf (rbt-color w) +rbt-color-red+)
                   (setf x (parent x)))

                 (progn
                   (if (= (rbt-color (right-child w)) +rbt-color-black+)
                     (progn
                       (setf (rbt-color (left-child w)) +rbt-color-black+
                             (rbt-color w) +rbt-color-red+)
                       (rotate-right tree w)
                       (setf w (right-child (parent x)))))

                   (setf (rbt-color w) (rbt-color (parent x))
                         (rbt-color (parent x)) +rbt-color-black+
                         (rbt-color (right-child w)) +rbt-color-black+)

                   (rotate-left tree (parent x))
                   (setf x (root tree)))))

             ;; ELSE
             (progn
               (setf w (left-child (parent x)))

               ;(format t "LC ~A " w)
               ;(break)
               (if (= (rbt-color w) +rbt-color-red+)
                 (progn
                   (setf (rbt-color w) +rbt-color-black+
                         (rbt-color (parent x)) +rbt-color-red+)
                   (rotate-right tree (parent x))
                   (setf w (left-child (parent x)))))

               (if (and (eq (rbt-color (right-child w)) +rbt-color-black+)
                        (eq (rbt-color (left-child w)) +rbt-color-black+))
                 (progn
                   (setf (rbt-color w) +rbt-color-red+)
                   (setf x (parent x)))

                 (progn
                   (if (= (rbt-color (left-child w)) +rbt-color-black+)
                     (progn
                       (setf (rbt-color (right-child w)) +rbt-color-black+
                             (rbt-color w) +rbt-color-red+)
                       (rotate-left tree w)
                       (setf w (left-child (parent x)))
                       ;(break)
                       ))

                   (setf (rbt-color w) (rbt-color (parent x))
                         (rbt-color (parent x)) +rbt-color-black+
                         (rbt-color (left-child w)) +rbt-color-black+)

                   (rotate-right tree (parent x))
                   (setf x (root tree)))))))

     (setf (rbt-color x) +rbt-color-black+)))

;;; Misc

(defmethod walk-tree-nodes ((node (eql *rbt-empty-node*)) walk-fn 
			    &optional (mode :inorder))
   "Special case..."
   (declare (ignore walk-fn mode)))

(defmethod walk-tree ((node (eql *rbt-empty-node*)) walk-fn &optional 
                      (mode :inorder))
  "Special case..."
  (declare (ignore walk-fn mode)))


(defmethod height ((node two-child-node))
   (let ((result 0))
     (loop while node do
           (incf result)
           (setf node (parent node)))
     result))

(defmethod height ((tree binary-search-tree))
   (let ((result 0))
     (walk-tree-nodes (root tree)
                      #'(lambda (n)
                          (let ((h (height n)))
                            (when (> h result)
                              (setf result h)))))
     result))


;;; Splay Tree
;;; 
;;; A splay tree implementation based on openmcl implementation
;;; and Goodrich and Tamassia "Algorithm Design"

(defmethod item-at ((tree binary-search-tree) &rest indexes)
  (declare (dynamic-extent indexes))
  (do* ((test (test tree))
        (sorter (sorter tree))
        (node (root tree)))
       ((or (null node)
	    (node-empty-p node)))
    (let ((key-of-node (funcall (key tree) (element node))))
      (if (funcall test (first indexes) key-of-node)
        (return node)
        (if (funcall sorter (first indexes) key-of-node)
          (setq node (left-child node))
          (setq node (right-child node)))))))


(defmethod update-element ((tree binary-search-tree) (value t) &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((node (item-at tree (first indexes))))
    (if node 
      (setf (element node) value)
      (warn "Tree does not contain node with index ~A" (first indexes)))
    node))

#+test
(update-element 
 (let ((tree (make-instance 'binary-search-tree :test #'= :key #'first
                            :sorter #'<)))
   (insert-item tree (make-bst-node '(1 one)))
   (insert-item tree (make-bst-node '(2 two)))
   (insert-item tree (make-bst-node '(3 three)))
   (insert-item tree (make-bst-node '(4 four))))
 'five 2) 


(defgeneric bst-node-is-left-child (node)
  (:documentation "Is this node the left child of its parent?")
  (:method ((node bst-node))
           (let ((parent (parent node)))
             (and (equal node (left-child parent)))))
  (:method (item)
           (declare (ignore item))))


(defgeneric bst-node-is-right-child (node)
  (:documentation "Is this node the right child of its parent?")
  (:method ((node bst-node))
           (let ((parent (parent node)))
             (and (equal node (right-child parent)))))
    (:method (item)
           (declare (ignore item))))


(defgeneric bst-node-set-right-child (node new-right)
  (:documentation "Set new-right as the right child of node")
  (:method ((node bst-node) (new-right bst-node))
           (when (setf (right-child node) new-right)
             (setf (parent new-right) node)))
  (:method ((node bst-node) item)
           (declare (ignore item))
           (setf (right-child node) nil)))

#+test
(bst-node-set-right-child (make-bst-node "node") nil)


(defgeneric bst-node-set-left-child (node new-left)
  (:documentation "Set new-left as the left child of node")
  (:method ((node bst-node) (new-left bst-node))
           (when (setf (left-child node) new-left)
             (setf (parent new-left) node)))
  (:method ((node bst-node) item)
           (declare (ignore item))
           (setf (left-child node) nil)))

#+test
(bst-node-set-left-child (make-bst-node "foo") nil)


(defgeneric bst-node-replace-child (node old-node new-node)
  (:documentation "Replace the child of this node.")
  (:method ((node bst-node) (old-node bst-node) (new-node bst-node))
           (if (equal old-node (left-child node))
             (bst-node-set-left-child node new-node)
             (bst-node-set-right-child node new-node))))
   

(defclass* splay-tree (binary-search-tree)
  ()
  (:default-initargs
    :key 'identity
    :test 'eq
    :sorter '<))


(defgeneric splay-tree-rotate (tree node)
  (:documentation "rotate the node (and maybe the parent) until the node is
the root of the tree")
  (:method ((tree binary-search-tree) (node bst-node))
           (when (and node (null (equal node (root tree))))
             (let* ((parent (parent node))
                    (grandparent (if parent (parent parent)))
                    (was-left (bst-node-is-left-child node)))
               (if grandparent
                 (bst-node-replace-child grandparent parent node)
                 (setf (root tree) node
                       (parent node) nil))
               (if was-left
                 (progn
                   (bst-node-set-left-child parent (right-child node))
                   (bst-node-set-right-child node parent))
                 (progn
                   (bst-node-set-right-child parent (left-child node))
                   (bst-node-set-left-child node parent))))))
  (:method ((tree binary-search-tree) item)
           (declare (ignore item))))


(defgeneric splay-tree-splay (tree node)
  (:documentation "Preform the splay operation on the tree about this node
rotating the node until it becomes the root")
  (:method ((tree binary-search-tree) item)
           (declare (ignore item)))
  (:method ((tree binary-search-tree) (node bst-node))
           (do* ()
                ((equal node (root tree)))
             (let* ((parent (parent node))
                    (grandparent (parent parent)))
               (cond ((null grandparent)
                      (splay-tree-rotate tree node)) ; node is now root
                     ((eq (bst-node-is-left-child node)
                          (bst-node-is-left-child parent))
                      (splay-tree-rotate tree parent)
                      (splay-tree-rotate tree node))
                     (t
                      (splay-tree-rotate tree node)
                      (splay-tree-rotate tree node)))))))


(defmethod insert-item :after ((tree splay-tree) (node bst-node))
  (splay-tree-splay tree node))
  

#+test
(let ((tree (make-instance 'splay-tree :test #'equal :key #'first
                          :sorter #'string-lessp)))
  (insert-item tree (make-bst-node '("s" south)))
  (insert-item tree (make-bst-node '("n" north)))
  (insert-item tree (make-bst-node '("e" east)))
  (insert-item tree (make-bst-node '("w" west))))
  

#+test
(let ((tree (make-instance 'splay-tree :test #'= :key #'first
                          :sorter #'<)))
  (insert-item tree (make-bst-node '(1 one)))
  (insert-item tree (make-bst-node '(2 two)))
  (insert-item tree (make-bst-node '(3 three)))
  (insert-item tree (make-bst-node '(4 four))))


(defmethod item-at ((tree splay-tree) &rest indexes)
  (declare (dynamic-extent indexes))
  (do* ((test (test tree))
        (sorter (sorter tree))
        (node (root tree)))
       ((null node))
    (let ((key-of-node (funcall (key tree) (element node))))
      (if (funcall test (first indexes) key-of-node)
        (progn 
          (splay-tree-splay tree node)
          (return node))
        (if (funcall sorter (first indexes) key-of-node)
          (setq node (left-child node))
          (setq node (right-child node)))))))


(defmethod update-element ((tree splay-tree) (value t) &rest indexes)
  (declare (dynamic-extent indexes))
  (let ((node (item-at tree (first indexes))))
    (if node 
      (setf (element node) value)
      (warn "Tree does not contain node with index ~A" (first indexes)))
    node))

#+test
(update-element 
 (let ((tree (make-instance 'splay-tree :test #'equal :key #'first
                            :sorter #'<)))
   (insert-item tree (make-bst-node '(1 one)))
   (insert-item tree (make-bst-node '(2 two)))
   (insert-item tree (make-bst-node '(3 three)))
   (insert-item tree (make-bst-node '(4 four))))
   'five 4)


(defmethod find-item ((tree splay-tree) (node bst-node))
  (do* ((test (test tree))
        (sorter (sorter tree))
        (key (key tree))
        (current (root tree)))
       ((null current))
    (let ((key-of-current (funcall key (element current)))
          (element-of-current (element current)))
      (if (and (funcall test (funcall key (element node))
                        key-of-current)
               (funcall test (element node) element-of-current))
        (progn 
          (splay-tree-splay tree current)
          (return current))
        (if (funcall sorter (funcall key (element node)) key-of-current)
          (setq current (left-child current))
          (setq current (right-child current)))))))

#+test
(find-item (let ((tree (make-instance 'splay-tree :test #'equal :key #'first
                          :sorter #'<)))
             (insert-item tree (make-bst-node '(1 one)))
             (insert-item tree (make-bst-node '(2 two)))
             (insert-item tree (make-bst-node '(3 three)))
             (insert-item tree (make-bst-node '(4 four))))
           '(3 three))

#+test
(find-item (let ((tree (make-instance 'splay-tree :test #'equal :key #'first
                                      :sorter #'string-lessp)))
             (insert-item tree (make-bst-node '("s" south)))
             (insert-item tree (make-bst-node '("n" north)))
             (insert-item tree (make-bst-node '("e" east)))
             (insert-item tree (make-bst-node '("w" west))))
           '("s" south))


(defgeneric right-most-child (node)
  (:documentation "Walk down the right side of the tree until a leaf node is
found, then return that node")
  (:method ((node bst-node))
           (if (right-child node)
             (right-most-child (right-child node))
             node)))


;;; must call find-item first to ensure proper amortized
;;; analysis - jjm
(defmethod delete-node ((tree splay-tree) (node bst-node))
  (if (find-item tree node)
    (let* ((old-root (root tree))
           (new-root (right-most-child (left-child old-root)))
           (new-root-parent (parent new-root)))
      (bst-node-set-right-child (parent new-root) nil)
      (bst-node-set-left-child new-root (left-child old-root))
      (bst-node-set-right-child new-root (right-child old-root))
      (setf (parent new-root) nil
            (root tree) new-root)
      (splay-tree-splay tree new-root-parent)
      old-root)
    (warn "Item ~A not found in splay-tree" node)))

(defmethod delete-item ((tree splay-tree) (item t))
  (delete-node tree (make-node-for-container tree item)))
      

#+test
(let ((tree (make-instance 'splay-tree
              :test #'equal :key #'first
              :sorter #'<)))
  (insert-item tree (make-bst-node '(3 three)))
  (insert-item tree (make-bst-node '(4 four)))
  (insert-item tree (make-bst-node '(5 five)))
  (insert-item tree (make-bst-node '(6 six)))
  (insert-item tree (make-bst-node '(7 seven)))
  (insert-item tree (make-bst-node '(8 eight)))
  (insert-item tree (make-bst-node '(10 ten)))
  (insert-item tree (make-bst-node '(11 eleven)))
  ;;(item-at tree 7)
  ;;(item-at tree 6)
  ;;(item-at tree 5)
  ;;(item-at tree 4)
  (item-at tree 3)
  (item-at tree 10)
  (item-at tree 8)
  (delete-item tree '(8 eight))
  tree)


(defmethod delete-item-at ((tree splay-tree) &rest indexes)
  (declare (dynamic-extent indexes))
  (delete-item tree (item-at tree (first indexes))))

#+test
(let ((tree (make-instance 'splay-tree
              :test #'equal :key #'first
              :sorter #'<)))
  (insert-item tree (make-bst-node '(3 three)))
  (insert-item tree (make-bst-node '(4 four)))
  (insert-item tree (make-bst-node '(5 five)))
  (insert-item tree (make-bst-node '(6 six)))
  (insert-item tree (make-bst-node '(7 seven)))
  (insert-item tree (make-bst-node '(8 eight)))
  (insert-item tree (make-bst-node '(10 ten)))
  (insert-item tree (make-bst-node '(11 eleven)))
  ;;(item-at tree 7)
  ;;(item-at tree 6)
  ;;(item-at tree 5)
  ;;(item-at tree 4)
  (item-at tree 3)
  (item-at tree 10)
  (item-at tree 8)
  (delete-item-at tree 8)
  tree)

  

#+test
(let ((tree (make-instance 'splay-tree :test #'= :key #'first
                          :sorter #'<)))
  (insert-item tree (make-bst-node '(1 one)))
  (insert-item tree (make-bst-node '(2 two)))
  (insert-item tree (make-bst-node '(3 three)))
  (insert-item tree (make-bst-node '(4 four))))

#+test
(item-at (let ((tree (make-instance 'splay-tree :test #'= :key #'first
                          :sorter #'<)))
  (insert-item tree (make-bst-node '(1 one)))
  (insert-item tree (make-bst-node '(2 two)))
  (insert-item tree (make-bst-node '(3 three)))
  (insert-item tree (make-bst-node '(4 four)))) 1)

#+test
(walk-tree 
 (root (let ((tree (make-instance 'splay-tree :test #'= :key #'first
                                  :sorter #'<)))
         (insert-item tree (make-bst-node '(1 one)))
         (insert-item tree (make-bst-node '(2 two)))
         (insert-item tree (make-bst-node '(3 three)))
         (insert-item tree (make-bst-node '(4 four)))
         (item-at tree 4)
         tree)) 
 #'(lambda (node)
     (format t "~%~A"
             node)))

;;; end splay tree

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************