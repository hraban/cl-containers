
(in-package containers)

(defgeneric sample-elements (container generator count)
  (:documentation "Return a list of count elements of the container uniformly at
random using the generator. The sampling is done _with_ replacement."))

;;; ---------------------------------------------------------------------------

(defgeneric sample-key (associative-container generator)
  (:documentation "Return an element from of the keys of the associative
container uniformly at random using the generator."))

;;; ---------------------------------------------------------------------------

(defgeneric sample-unique-elements (container generator count)
  (:documentation "Return a list of count elements from the container 
sampled uniformly at random without replacement."))

;;; ---------------------------------------------------------------------------

(defgeneric sample-element (container generator)
  (:documentation "Return an element of the container uniformly at random using
the generator."))

;;; ---------------------------------------------------------------------------

(defgeneric sample-item (container generator)
  (:documentation "Return an item of the container uniformly at random using
the generator. Same as sample-element unless container is of type container-uses-nodes-mixin."))

;;; ---------------------------------------------------------------------------

(defgeneric best-item (items function &key key test filter)
  (:documentation "Returns the item in items with the 'best' value of function where
'best' is determined by test. You can use filter to limit which items are compared and
key to select out what part of each item actually gets tested. See argmin and argmax 
for specializations."))

;;; ---------------------------------------------------------------------------

(defgeneric argmax (items function &key key filter)
  (:documentation "Returns the item in items with the biggest (#'>) value of function. See best-item for a generalization.  See best-item for a generalization."))

;;; ---------------------------------------------------------------------------

(defgeneric argmin (items function &key key filter)
  (:documentation "Returns the item in items with the smallest (#'<) value of function."))

;;; ---------------------------------------------------------------------------

(defgeneric make-node-for-container (container item &key))

;;; ---------------------------------------------------------------------------

(defgeneric node-empty-p (node))

;;; ---------------------------------------------------------------------------

(defgeneric size (abstract-container)
  (:documentation "Returns the number of items currently in the container."))

;;; ---------------------------------------------------------------------------

(defgeneric empty! (abstract-container)
  (:documentation "Removes all items from the container and returns nil."))

;;; ---------------------------------------------------------------------------

(defgeneric some-item-p (container predicate)
  (:documentation "Returns the first item in the container for which predicate
holds. Predicate should be a function of one argument for iteratable containers
and a function of two arguments for associative containers."))

;;; ---------------------------------------------------------------------------

(defgeneric every-item-p (container predicate)
  (:documentation "Returns true if every item in the container satisfies the 
predicate. Predicate should be a function of one argument for iteratable containers
and a function of two arguments for associative containers."))

;;; ---------------------------------------------------------------------------

(defgeneric make-container-for-contents (container &rest args)
  (:documentation "Creates a sub-container to be used as contents for a 
super-container."))

;;; ---------------------------------------------------------------------------

(defgeneric nth-element (container index)
  (:documentation "Returns the nth element in the container's 'natural' order."))

;;; ---------------------------------------------------------------------------

(defgeneric nth-item (container index)
  (:documentation "Returns the nth item in the container's 'natural' order. This is the same as nth-element unless the contaienr is of type container-uses-nodes-mixin."))

;;; ---------------------------------------------------------------------------

(defgeneric total-size (bounded-container-mixin))

;;; ---------------------------------------------------------------------------

(defgeneric item-at (indexed-container-mixin &rest indexes)
  (:documentation "Returns the item specified by the indexes."))

;;; ---------------------------------------------------------------------------

(defgeneric item-at! (indexed-container-mixin value &rest indexes)
  (:documentation "[Destructively] modifies the item specified by the 
indexes to the value."))

;;; ---------------------------------------------------------------------------

(defgeneric find-item (findable-container-mixin item)
  (:documentation "Find item in container using the container's test
method for comparisons. The test method must take two parameters. The
first will be the item being searched for; the second will be an item
in the container. If the container has a key (keyed-container-mixin),
then the test is performed on the item and the key of each element
in the container. Find-item returns nil if the item is not found and it
returns the element in the container if it is."))

;;; ---------------------------------------------------------------------------

(defgeneric find-node (findable-container-mixin thing)
  (:documentation "Find node containing thing in container using the container's test
method for comparisons. The test method must take two parameters. The
first will be the item being searched for; the second will be an item
in the container. If the container has a key (keyed-container-mixin),
then the test is performed on the item and the key of each element
in the container. Find-item returns nil if the thing is not found and it
returns the node in the container if it is. Find-node is the same as find-element
for containers that do not use nodes."))

;;; ---------------------------------------------------------------------------

(defgeneric find-element (findable-container-mixin thing)
  (:documentation "For now, compare find-item."))

;;; ---------------------------------------------------------------------------

(defgeneric search-for-item (container item &key test key)
  (:documentation "Hunt for the item in the container. Key and Test 
are as in member."))

;;; ---------------------------------------------------------------------------

(defgeneric search-for-match (container predicate &key key)
  (:documentation "Hunt for an item in the container that satisfies 
the predicate. Key is as in count-if."))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-nodes (iteratable-container-mixin function)
  (:documentation "Applies function to each node in the container. If the container doesn't have nodes, then this is equivalent to iterate-elements."))

;;; ---------------------------------------------------------------------------

(defgeneric print-container (iteratable-container-mixin &optional stream)
  (:documentation "Prints the contents of container (using PRINT). Returns the container."))

;;; ---------------------------------------------------------------------------

(defgeneric collect-nodes (container &key filter transform)
  (:documentation "Returns a possibly filtered and possibly transformed list
of the nodes in a container. If the container uses nodes, then the items are
the nodes. If not, collect-nodes is equivalent to collect-elements."))

;;; ---------------------------------------------------------------------------

(defgeneric collect-elements (container &key filter transform)
  (:documentation "Returns a possibly filtered and possibly transformed list
of the elements in a container. If the container uses nodes, then the elements are
the things 'in' the nodes."))

;;; ---------------------------------------------------------------------------

(defgeneric collect-key-value (container &key filter transform)
  (:documentation "Iterate over the keys and values of the container and 
return a list of the ones that pass the filter function transformed by
the transform function."))

;;; ---------------------------------------------------------------------------

(defgeneric collect-keys (container &key filter transform)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric iterate-key-value (container function))

(defgeneric iterate-value-key (container function))

;;?? should be settable
(defgeneric first-item (ordered-container-mixin))
;;?? should be settable
(defgeneric last-item (ordered-container-mixin))

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

;;; ---------------------------------------------------------------------------

(defgeneric reverse-find (contents-as-hashtable-mixin value &key test)
  (:documentation "Finds the key in the associative-container whose 
value is VALUE."))

(defgeneric enqueue (abstract-queue item))
(defgeneric dequeue (abstract-queue))

(defgeneric pop-item (abstract-stack))

(defgeneric children (many-child-node))

;;; ---------------------------------------------------------------------------

(defgeneric ensure-sorted (container)
  (:documentation "This method ensures that the sorted-list-container is sorted,
and then returns the container."))

;;; ---------------------------------------------------------------------------

(defgeneric force-sort (container)
  (:documentation "This method forces a sort on the next pertinent access of
the container."))

;;; ---------------------------------------------------------------------------

(defgeneric remove-items-if (container test)
  (:documentation "Removes items from a container that satisfy the test. The
container is returned."))

;;; ---------------------------------------------------------------------------

(defgeneric container->array (ordered-container-mixin))
(defgeneric element-position (ordered-container-mixin element &key test key)
  (:documentation "Returns the position of element in container using test and
key to match. Key defaults to identity and test defaults to eq."))
(defgeneric delete-item (ordered-container-mixin item))
(defgeneric delete-item-if (ordered-container-mixin test))

;;; ---------------------------------------------------------------------------

(defgeneric reverse-container (ordered-container-mixin)
  (:documentation "Destructively alters the elements/nodes of an ordered container so that they are reversed."))
  
  