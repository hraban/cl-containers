(in-package #:containers)



#|

;;; stack with a list

(defclass* stack-using-list (abstract-stack contents-as-list-mixin)
   ((contents
     :initform nil
     :accessor contents
     )))

(defmethod first-item ((container abstract-stack))
   (first (contents container)))

(defmethod pop-item ((stack stack-using-list))
   (pop (contents stack)))


;;; bag-list

(defclass* bag-list (bag-container-abstract contents-as-list-mixin)
   ())


(defmethod delete-item ((container bag-list) item)
   (setf (contents container)
         (delete item (contents container)))
   item)


;;; bag-hash

(defclass* bag-hash (bag-container-abstract contents-as-hashtable-mixin)
   ())


(defmethod delete-item ((container bag-hash) item)
   (remhash item (contents container))
   item)

(defmethod insert-item ((container contents-as-hashtable-mixin) item)
   (setf (gethash item (contents container)) item))



(defgeneric smallest-item (sorted-container-mixin))
(defgeneric biggest-item (sorted-container-mixin))
|#

#|
Container Notes


u::abstract-container
make-container
size
empty-p
empty-container


u::bounded-container-mixin
total-size


u::indexed-container-mixin (and associative-container-mixin)
item-at
item-at!


u::iteratable-container-mixin
iterate-container
collect-elements (basic version using iterate-container)
? search-for-item (container item :key :test)
? first-match (container :key :test)


u::findable-container-mixin
;;?? Needs key and test
find-item (findable-container-mixin item)


u::ordered-container-mixin
(defgeneric first-item (ordered-container-mixin))
(defgeneric item-after (ordered-container-mixin item))
(defgeneric item-before (ordered-container-mixin item))
(defgeneric last-item (ordered-container-mixin))
insert-item
insert-sequence
insert-list


u::sorted-container-mixin
(defgeneric successor (sorted-container-mixin item))
(defgeneric predecessor (sorted-container-mixin item))


u::contents-as-hashtable-mixin
collect-keys
iterate-key-value


u::abstract-queue
dequeue
enqueue == insert-item


u::abstract-stack
first-item (settable)
pop-item
push-item


test (in bst, make part of searchable; make keyed part of searchable?)
height (of a tree)

delete-item
delete-item-if

iterate-key-value doesn't make sense for nested hash-tables, we 
should implement both
   nested and non-nested...
get rid of empty, make everyone type empty-container or, better yet, empty!

aided and unaided searching... I can search any iteratable container 
but I can search
bst's fast.

maybe find-item and search-for-item (the slower and more flexible 
one) hunt-for-item, lookup-item vs find-item

? append-item
? delete-last


|#

#|
Todo
----

* Re-design class hierarchy
* queue-on-container
* create heap class and hide EKSL heap in it, then use this for heap based PQ
* sets and bags need :test, :key in the constructor
* the X-container adaptors have very similar implementation, make 
this a sub-class (adaptive-container-mixin)
   to handle most of the pattern.

* sparse-array should use associative-container (nested hash tables) 
and be able
   to iterate container
* arrays should allow for initial-contents
* add associative-arrays
* add initial-element-fn
* sparse-array-container should use contents-as-hashtable-mixin (and 
have size the amount in use?)
* restore regular associative-container and created 
nested-associative-container
* doubly-linked-list
* add methods for working with the end of an ordered-container (and a 
new class)
* break ordered container into pieces corresponding to efficiency classes
* delete-first, delete-last, append-item and so on
* generic operations for standard lisp types? (container ops are 
destructive... makes this hard)

** fix / improve bags and add sets
** we should have stack-container and bag-container and so on.
** remove smallest-item and biggest-item (replace with first-item and 
last-item)
** dump generic-iteratable-container-mixin

* rework observable stuff and add to eksl-utilities

* copy-container
* samep (good use of forward-iterators)

* generic algoritms (e.g., sort, merge, find...)??
* instead of insert-list, insert-sequence, and so on... have 
insert-object and dispatch on it

More esoteric data structures
   Fibonacci heaps (and other heaps)
   etc.

Less esoteric data structures

??

bag-container / set-container using lists, hash-tables or other 
things. The problem
is that the bag/set-container seens to require specialization on 
bag/set. The other
problem is that a bag-container using a hash-table should implement a 
fast find-item
rather than the iterative search-for-item.

|#

#|
Do we want to add assertions and such (use a parameter or a mixin)

Westy's slow-mixin

Look at various Lisp list operations (e.g., count, replace, and so on)
and see if they would make sense for any sorts of containers.

Setf's
|#

#|
 From EKSL priority queue

            Copy-queue
            queue-length

	   priority-queue-head-or-nil ; = front unless queue is empty -> nil
	   priority-queue-delete
	   priority-queue-find
	   priority-queue-map-in-priority-order
	   priority-queue-map-in-heap-order
	   priority-queue-elements
	   print-priority-queue-elements
	   do-priority-queue
	   reorder-priority-queue))

#+Ignore
(defmacro declare-inline (function-names)
   (dolist (name function-names)
     `(declaim (inline ,name))))

(defmacro declaim-inline (function-name)
   `(declaim (inline ,function-name)))

|#

(in-package #:u)

(defun test-container-class (container-class &rest args)
   (let ((c (apply #'make-container container-class args)))
     (test)

     ))

(defun container
        ))



(defun test-basic-container-operations (class)
   (let ((c (make-container class)))
     (insert-item c 1)
     (insert-item c 2)
     (insert-item c 3)
     (ensure (= (size c) 3))
     (empty! c)
     (ensure (= (size c) 0))
     (ensure (empty-p c))
     (insert-list c '(2 3))
     (ensure (= (size c) 2))))

(test-basic-container-operations 'dlist-container)
(test-basic-container-operations 'priority-queue-heap)

(mapc (lambda (c)
         (print c)
         (test-basic-container-operations c))
       (DETERMINE-CONCRETE-CONTAINER-SUBCLASSES 'ordered-container-mixin))


(SET-CONTAINER BAG-CONTAINER STACK-CONTAINER PRIORITY-QUEUE-HEAP
  DLIST-CONTAINER BASIC-QUEUE LIST-CONTAINER ALIST-CONTAINER
  SPARSE-ARRAY-CONTAINER RING-BUFFER VECTOR-CONTAINER ARRAY-CONTAINER
  ABSTRACT-FORCE-SIMULATOR-DOMAIN::FORCE-FLOW-GRID MBR HEAP-CONTAINER
  BINARY-SEARCH-TREE RED-BLACK-TREE PRIORITY-QUEUE-ON-CONTAINER bag/set-container
  ASSOCIATIVE-CONTAINER KEYED-ASSOCIATIVE-CONTAINER)
|#


#|
(setf a (make-container 'alist-container))
(setf (item-at a :a :b) 1
      (item-at a :b :c) 2
      (item-at a :c :d) 3
      (item-at a :d :e) 4)
(delete-item-at a :a :b)
(iterate-key-value a (lambda (k v) (format t "~%~A ~A" k v)))
(iterate-keys a (lambda (k) (format t "~%~A" k)))

(setf a (make-container 'alist-container))
(setf (item-at a :a) 1
      (item-at a :b) 2
      (item-at a :c) 3
      (item-at a :d) 4)
(delete-item-at a :a)
(iterate-key-value a (lambda (k v) (format t "~%~A ~A" k v)))
(iterate-keys a (lambda (k) (format t "~%~A" k)))
|#

#| Perhaps useful for documentation
(let ((c-1 (make-container 'associative-container))
      (c-2 (make-container 'associative-container 
                           :initial-element 2))
      (c-3 (make-container 'associative-container
                           :initial-element (random 10)))
      (c-4 (make-container 'associative-container
                           :initial-element-fn (lambda () (random 10)))))
  
  (loop for c in (list c-1 c-2 c-3 c-4)
        for name in (list "Basic Table" "Initial-element" 
                          "Random Element" "Element Function") do
        (format t "~%~%~A:" name)
        (setf (item-at c :a) 1)
        (format t "~%We get out what we put in.")
        (format t "~%(item-at c :a) ==> ~A" (item-at c :a))
        (format t "~%What do we get when we don't put anything in?")
        (format t "~%(item-at c :b) ==> ~A" (item-at c :b))
        (format t "~%We get the same thing each time ~
                   (even if we didn't add it ourselves).")
        (format t "~%(item-at c :b) ==> ~A" (item-at c :b))
        (format t "~%What happens if we look somewhere else? ~
                   (why initial-element-fn exists).")
        (format t "~%(item-at c :c) ==> ~A" (item-at c :c))))
|#