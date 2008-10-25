
(in-package #:containers)

(defgeneric sample-elements (container generator count)
  (:documentation "Return a list of count elements of the container uniformly at
random using the generator. The sampling is done _with_ replacement."))


(defgeneric sample-key (associative-container generator)
  (:documentation "Return an element from of the keys of the associative
container uniformly at random using the generator."))


(defgeneric sample-unique-elements (container generator count)
  (:documentation "Return a list of count elements from the container 
sampled uniformly at random without replacement."))


(defgeneric sample-element (container generator)
  (:documentation "Return an element of the container uniformly at random using
the generator."))


(defgeneric sample-item (container generator)
  (:documentation "Return an item of the container uniformly at random using
the generator. Same as sample-element unless container is of type container-uses-nodes-mixin."))


(defgeneric best-item (items function &key key test filter)
  (:documentation "Returns the item in items with the 'best' value of function where
'best' is determined by test. You can use filter to limit which items are compared and
key to select out what part of each item actually gets tested. See argmin and argmax 
for specializations."))


(defgeneric argmax (items function &key key filter)
  (:documentation "Returns the item in items with the biggest (#'>) value of function. See best-item for a generalization.  See best-item for a generalization."))


(defgeneric argmin (items function &key key filter)
  (:documentation "Returns the item in items with the smallest (#'<) value of function."))


(defgeneric size (abstract-container)
  (:documentation "Returns the number of items currently in the container."))


(defgeneric empty! (abstract-container)
  (:documentation "Removes all items from the container and returns nil."))


(defgeneric some-item-p (container predicate)
  (:documentation "Returns the first item in the container for which predicate
holds. Predicate should be a function of one argument for iteratable containers
and a function of two arguments for associative containers."))


(defgeneric every-item-p (container predicate)
  (:documentation "Returns true if every item in the container satisfies the 
predicate. Predicate should be a function of one argument for iteratable containers
and a function of two arguments for associative containers."))


(defgeneric make-container-for-contents (container &rest args)
  (:documentation "Creates a sub-container to be used as contents for a 
super-container."))


(defgeneric nth-element (container index)
  (:documentation "Returns the nth element in the container's 'natural' order."))


(defgeneric nth-item (container index)
  (:documentation "Returns the nth item in the container's 'natural' order. This is the same as nth-element unless the contaienr is of type container-uses-nodes-mixin."))


(defgeneric item-at (indexed-container-mixin &rest indexes)
  (:documentation "Returns the item specified by the indexes."))


(defgeneric (setf item-at) (value container &rest indexes)
  (:documentation ""))


(defgeneric item-at! (indexed-container-mixin value &rest indexes)
  (:documentation "[Destructively] modifies the item specified by the 
indexes to the value."))


(defgeneric (setf item-at-1) (value container index)
  (:documentation ""))


(defgeneric find-item (findable-container-mixin item)
  (:documentation "Find item in container using the container's test
method for comparisons. The test method must take two parameters. The
first will be the item being searched for; the second will be an item
in the container. If the container has a key (keyed-container-mixin),
then the test is performed on the item and the key of each element
in the container. Find-item returns nil if the item is not found and it
returns the element in the container if it is."))


(defgeneric find-node (findable-container-mixin thing)
  (:documentation "Find node containing thing in container using the container's test
method for comparisons. The test method must take two parameters. The
first will be the item being searched for; the second will be an item
in the container. If the container has a key (keyed-container-mixin),
then the test is performed on the item and the key of each element
in the container. Find-item returns nil if the thing is not found and it
returns the node in the container if it is. Find-node is the same as find-element
for containers that do not use nodes."))


(defgeneric find-element (findable-container-mixin thing)
  (:documentation "For now, compare find-item."))


(defgeneric search-for-item (container item &key test key)
  (:documentation "Hunt for the item in the container. Key and Test 
are as in member."))


(defgeneric search-for-match (container predicate &key key)
  (:documentation "Hunt for an item in the container that satisfies 
the predicate. Key is as in count-if."))


(defgeneric iterate-nodes (iteratable-container-mixin function)
  (:documentation "Applies function to each node in the container. If the container doesn't have nodes, then this is equivalent to iterate-elements."))


(defgeneric print-container (iteratable-container-mixin &optional stream)
  (:documentation "Prints the contents of container (using PRINT). Returns the container."))


(defgeneric collect-nodes (container &key filter transform)
  (:documentation "Returns a possibly filtered and possibly transformed list
of the nodes in a container. If the container uses nodes, then the items are
the nodes. If not, collect-nodes is equivalent to collect-elements."))


(defgeneric collect-elements (container &key filter transform)
  (:documentation "Returns a possibly filtered and possibly transformed list of the elements in a container. If the container uses nodes, then the elements are the things 'in' the nodes. Warning: it is possible for the result to share structure with the original container."))


(defgeneric collect-key-value (container &key filter transform)
  (:documentation "Iterate over the keys and values of the container and 
return a list of the ones that pass the filter function transformed by
the transform function."))


(defgeneric collect-keys (container &key filter transform)
  (:documentation "Collects the `keys` of a container into a list.

The `filter` and `transform` arguments should be `nil` or 
functions of one argument. If `filter` is non-nil, then the 
list returned will contain only `keys` that return true. If
transform is non-nil, then it will be applied to each key
that passes the filter."))


(defgeneric iterate-value-key (container function))

(defgeneric (setf first-element) (x y)
  (:documentation ""))

(defun first-item (x) (first-element x))

(defun last-item (x) (last-element x))

(defgeneric delete-first (ordered-container-mixin)
  (:documentation "Removes (and returns) the first item in an ordered 
container.
Behavior is undefined if the container is empty."))

(defgeneric delete-last (ordered-container-mixin)
  (:documentation "Removes (and returns) the last item in an ordered 
container.
Behavior is undefined if the container is empty."))

(defgeneric insert-item (non-associative-container-mixin item)
  (:documentation "Adds item to the container"))

(defgeneric append-item (ordered-container item)
  (:documentation "Add an item to the end of an ordered container."))

(defgeneric insert-new-item (non-associative-container-mixin item &key test key)
  (:documentation "Adds item to the container unless it is already there"))

(defgeneric append-new-item (ordered-container item &key test key)
  (:documentation "Add an item to the end of an ordered container unless its
already there."))

(defgeneric insert-sequence (non-associative-container-mixin sequence)
  (:documentation "Adds each item in the sequence to the container in an
upspecified order."))

(defgeneric insert-list (non-associative-container-mixin list)
  (:documentation "Adds each item in the list to the container in an
upspecified order."))

(defgeneric delete-list (non-associative-container-mixin list)
  (:documentation "Deletes each item in the list from the container."))


(defgeneric reverse-find (contents-as-hashtable-mixin value &key test)
  (:documentation "Finds the key in the associative-container whose 
value is VALUE."))


(defgeneric ensure-sorted (container)
  (:documentation "This method ensures that the sorted-list-container is sorted,
and then returns the container."))


(defgeneric force-sort (container)
  (:documentation "This method forces a sort on the next pertinent access of
the container."))


(defgeneric remove-items-if (container test)
  (:documentation "Removes items from a container that satisfy the test. The
container is returned."))


(defgeneric container->array (ordered-container-mixin))
(defgeneric element-position (ordered-container-mixin element &key test key)
  (:documentation "Returns the position of element in container using test and
key to match. Key defaults to identity and test defaults to eq."))


(defgeneric reverse-container (ordered-container-mixin)
  (:documentation "Destructively alters the elements/nodes of an ordered container so that they are reversed."))


(defgeneric unique-elements (container &key key)
  (:documentation ""))


(defgeneric unique-nodes (container &key key)
  (:documentation ""))
  
(defgeneric add-default-item (object &rest indexes)
  (:documentation ""))

(defgeneric add-initial-contents (object initial-contents)
  (:documentation ""))

(defgeneric best-element (container function &key key test filter)
  (:documentation ""))

(defgeneric best-node (container function &key key test filter)
  (:documentation ""))

(defgeneric biggest-item (heap)
  (:documentation ""))

(defgeneric children (many-child-node)
  (:documentation ""))

(defgeneric clean-up (container)
  (:documentation ""))

(defgeneric collect-elements-stably (container &key filter transform)
  (:documentation ""))

(defgeneric collect-items (object &rest args &key filter transform)
  (:documentation ""))

(defgeneric collect-key-value-stably (container)
  (:documentation ""))

(defgeneric combine-elements (iterator)
  (:documentation ""))

(defgeneric container-difference (c1 c2 &key key1 key2 )
  (:documentation ""))

(defgeneric container-dimension (container dimension)
  (:documentation ""))

(defgeneric count-elements (container item &key key test)
  (:documentation ""))

(defgeneric count-elements-if (container test &key key)
  (:documentation ""))

(defgeneric count-items (container item &key key test)
  (:documentation ""))

(defgeneric delete-biggest-item (heap)
  (:documentation ""))

(defgeneric delete-element (q item)
  (:documentation ""))

(defgeneric delete-item (ordered-container-mixin item)
  (:documentation ""))

(defgeneric delete-item-after (list node)
  (:documentation ""))

(defgeneric delete-item-at (container &rest indexes)
  (:documentation ""))

(defgeneric delete-item-before (list node)
  (:documentation ""))

(defgeneric delete-item-if (ordered-container-mixin test)
  (:documentation ""))

(defgeneric delete-node (tree node)
  (:documentation ""))

(defgeneric dequeue (abstract-queue)
  (:documentation ""))


#+(or)
;; no, it's a slot
(defgeneric dimensions (x)
  (:documentation ""))

#+(or)
;; no, it's a slot
(defgeneric element (x)
  (:documentation ""))

#+(or)
;; no, it's a slot
(defgeneric (setf element) (x y)
  (:documentation ""))


(defgeneric enqueue (abstract-queue item)
  (:documentation ""))

(defgeneric error-if-queue-empty (q &optional message &rest rest)
  (:documentation ""))

(defgeneric every-element-p (array predicate)
  (:documentation ""))

(defgeneric every-key-value-p (container predicate)
  (:documentation ""))

(defgeneric exchange-heap-nodes (n1 n2 heap)
  (:documentation ""))

(defgeneric find-child-node (node child &key test key)
  (:documentation ""))

(defgeneric find-set (item)
  (:documentation ""))

(defgeneric find-value (container item)
  (:documentation ""))

(defgeneric first-element (x)
  (:documentation ""))

(defgeneric graft-nodes (node1 node2)
  (:documentation ""))

(defgeneric has-children-p (node)
  (:documentation ""))

(defgeneric heap-node-parent (node heap)
  (:documentation ""))

(defgeneric heapify (heap node)
  (:documentation ""))

(defgeneric height (tree)
  (:documentation ""))

(defgeneric increment-end (container)
  (:documentation ""))

(defgeneric initialize-container (container)
  (:documentation ""))

(defgeneric inorder-walk (tree walk-fn)
  (:documentation ""))

(defgeneric inorder-walk-nodes (tree walk-fn)
  (:documentation ""))

(defgeneric insert-item-after (list node new-node)
  (:documentation ""))

(defgeneric insert-item-at (container item index)
  (:documentation "Inserts item at the specified index, increasing the index of all following elements"))

(defgeneric insert-item-before (list node new-node)
  (:documentation ""))

(defgeneric insert-item-ordered (list new-node)
  (:documentation ""))

(defgeneric insert-item-ordered-about-node (list node new-node)
  (:documentation ""))

(defgeneric item-at-1 (container index)
  (:documentation ""))

(defgeneric item-at-1! (hash-table value index)
  (:documentation ""))

(defgeneric item-key (container &rest indexes)
  (:documentation ""))

(defgeneric iterate-container (iterator fn)
  (:documentation ""))

(defgeneric iterate-elements (graph fn)
  (:documentation ""))

(defgeneric iterate-elements-stably (container fn)
  (:documentation ""))

(defgeneric iterate-key-value (container function)
  (:documentation ""))

(defgeneric iterate-key-value-stably (container fn)
  (:documentation ""))

(defgeneric iterate-keys (container function)
  (:documentation ""))

(defgeneric iterate-left (list item fn &optional inclusive?)
  (:documentation ""))

(defgeneric iterate-left-nodes (list item fn)
  (:documentation ""))

(defgeneric iterate-nodes-about-node (list item left-fn right-fn)
  (:documentation ""))

(defgeneric iterate-right (list item fn &optional inclusive?)
  (:documentation ""))

(defgeneric iterate-right-nodes (list item fn)
  (:documentation ""))

(defgeneric l-child (node heap)
  (:documentation ""))

(defgeneric l-child-index (node)
  (:documentation ""))

(defgeneric last-element (x)
  (:documentation ""))

(defgeneric (setf last-element) (x y)
  (:documentation ""))

(defgeneric left-and-right-nodes-for-item (list item)
  (:documentation ""))

(defgeneric left-node-for-item (list item)
  (:documentation ""))

(defgeneric link-nodes (node1 node2)
  (:documentation ""))

(defgeneric make-initial-element (container)
  (:documentation ""))

(defgeneric make-node-for-container (container item &key)
  (:documentation ""))

(defgeneric make-set (item)
  (:documentation ""))

(defgeneric next-item (x)
  (:documentation ""))

(defgeneric node-empty-p (node)
  (:documentation ""))

(defgeneric node-parent-index (node)
  (:documentation ""))

(defgeneric pop-item (abstract-stack)
  (:documentation ""))

(defgeneric postorder-walk (tree walk-fn)
  (:documentation ""))

(defgeneric postorder-walk-nodes (tree walk-fn)
  (:documentation ""))

(defgeneric predecessor (sorted-container-mixin item)
  (:documentation "Return the item that comes *before* item in the container. Only makes sense for sorted containers. Raises an element-not-found-error if the item isn't present in the container."))

(defgeneric preorder-walk (tree walk-fn)
  (:documentation ""))

(defgeneric preorder-walk-nodes (tree walk-fn)
  (:documentation ""))

(defgeneric print-container-contents (container stream)
  (:documentation ""))

(defgeneric print-container-summary (container stream)
  (:documentation ""))

(defgeneric push-item (stack item)
  (:documentation ""))

(defgeneric r-child (node heap)
  (:documentation ""))

(defgeneric r-child-index (node)
  (:documentation ""))

(defgeneric rb-delete-fixup (tree x)
  (:documentation ""))

(defgeneric reduce-container (container function &rest args &key key initial-value start end)
  (:documentation ""))

(defgeneric reduce-elements (container function &key 
                                       key initial-value start end)
  (:documentation ""))

(defgeneric reduce-nodes (container function &key key initial-value start end)
  (:documentation ""))

(defgeneric replace-item (list node start-item &key length finish-item)
  (:documentation ""))

(defgeneric representative (container item)
  (:documentation ""))

(defgeneric representative-node (container item)
  (:documentation ""))


(defgeneric right-node-for-item (list item)
  (:documentation ""))

(defgeneric rotate-left (tree x)
  (:documentation ""))

(defgeneric rotate-right (tree x)
  (:documentation ""))

(defgeneric search-for-element (list item &key test key)
  (:documentation ""))

(defgeneric search-for-key (container key-to-find &key test key)
  (:documentation ""))

(defgeneric search-for-matching-node (container predicate &key key)
  (:documentation ""))

(defgeneric search-for-node (container item &key test key)
  (:documentation ""))

(defgeneric search-for-node* (container item &key test key)
  (:documentation ""))

(defgeneric set-dirty-flag (container flag)
  (:documentation ""))

(defgeneric some-element-p (array predicate)
  (:documentation ""))

(defgeneric some-key-value-p (container predicate)
  (:documentation ""))

(defgeneric sort-container (container sorter key-fn)
  (:documentation ""))

(defgeneric sort-elements (container sorter &key key)
  (:documentation ""))

(defgeneric sort-keys (container sorter &key key)
  (:documentation ""))

(defgeneric sort-update-left (list node)
  (:documentation ""))

(defgeneric sort-update-right (list node)
  (:documentation ""))

(defgeneric successor (sorted-container-mixin item)
  (:documentation "Return the item that comes after item in the container. Only makes sense for sorted containers. Raises an element-not-found-error if the item isn't present in the container."))

(defgeneric total-size (x)
  (:documentation ""))

(defgeneric update-element (tree value &rest indexes)
  (:documentation ""))

(defgeneric update-item (list item)
  (:documentation ""))

(defgeneric walk-tree (node walk-fn &optional mode)
  (:documentation ""))

(defgeneric walk-tree-nodes (node walk-fn &optional mode)
  (:documentation ""))

(defgeneric (setf packages) (value container)
  (:documentation ""))

(defgeneric container->list (graph)
  (:documentation ""))

(defgeneric current-item (container)
  (:documentation ""))

(defgeneric iterate-children (node fn)
  (:documentation "Calls `fn` on every child of `node`."))

(defgeneric first-node (container)
  (:documentation ""))

(defgeneric %operate-after-finding (container element operation)
  )

(defgeneric insert-initial-contents-p (container)
  (:documentation "Returns true if this container type should rely on the default behavior of basic-initial-contents-mixin."))

(defgeneric iteratable-p (thing)
  (:documentation "Returns true if thing knows how to iterate-nodes.")
  (:method ((thing t))
           (values nil)))

(defgeneric key-value-iteratable-p (thing)
  (:documentation "Returns true if thing knows how to iterate-nodes.")
  (:method ((thing t))
           (values nil)))

