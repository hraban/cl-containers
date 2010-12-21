(in-package #:common-lisp-user)

(defpackage #:metabang.cl-containers
  (:use #:common-lisp)
  (:nicknames #:cl-containers #:containers)
  (:documentation
   "A library of container classes and algorithms for Common Lisp.")
  (:import-from #:metatilities
                #:deprecated
                #:defclass*
                #:defcondition
                #:ensure-list
                #:length-at-most-p 
                #:length-at-least-p
                #:length-1-list-p
                #:maparray
                #:samep
                #:set-equal
                #:apply-if-exists
		#:funcall-if-exists

                #:*samep-test*
                #:parent
                #:element
                #:argmax
                #:argmin
                #:best-item
                #:filter

                #:size
                #:root
                #:next-element
                #:total-size
                #:element-type

		#:class-precedence-list
		#:class-direct-subclasses
		#:get-class
		#:finalize-class-if-necessary
		#:muffle-redefinition-warnings)
  
  (:export #:basic-queue
           #:ring-buffer
           #:vector-container
           #:bounded-vector-container
           #:flexible-vector-container
           #:bag-container
           #:set-container
           #:list-container
           #:sorted-list-container
           #:dlist-container
           #:dlist-container-node
           #:stack-container
           
           #:binary-search-tree
           #:red-black-tree
           #:quad-tree
           #:rooted-tree-container
           #:many-child-node
           #:many-unordered-child-node 
           #:many-ordered-child-node
           #:parent-node-mixin
           
           #:contents-as-array-mixin
           #:contents-as-hashtable-mixin
           #:contents-as-list-mixin
           #:test-container-mixin
           
           #:priority-queue-on-container
           #:priority-queue-heap
           
           #:array-container
           #:sparse-array-container
           #:simple-associative-container
           #:associative-container
           #:alist-container
           
           #:biassociative-container-mixin
           #:associative-container-mixin
           #:keyed-associative-container
           
           #:heap-container
           #:abstract-queue
           #:k-best-heap-container
           
           #:stable-associative-container
           #:iteratable-container-mixin
           #:key-value-iteratable-container-mixin
           
           #:set-container
           #:bag-container
           
           #:union-find-container
           #:package-container 
           #:list-iterator
           )
  
  ;; Basic container operations
  (:export insert-item
           insert-new-item
           insert-list
           insert-sequence
           delete-list
           delete-item
           delete-item-if
           delete-node
           delete-element
           
           size
           empty-p
           empty!
           
           find-item
           find-node
	   find-successor-node
           find-element
           search-for-key
           search-for-item
           search-for-node
           search-for-node*
           search-for-match
           search-for-element
           
           predecessor
           successor
           
           copy-container
           container->list
           container->array
           
           total-size                    ; bounded-container-mixin
           
           iteratable-p
           key-value-iteratable-p
           
           ;;?? deprecated
           iterate-container             ; iteratable-container-mixin
           collect-items                 ; iteratable-container-mixin
           
           iterate-nodes
           iterate-elements
           collect-elements              ; iteratable-container-mixin
           collect-nodes
           
           collect-key-value             ; contents-as-hashtable-mixin
           collect-keys
           
           iterate-children              ; many-child-nodes
           
           #:reduce-container
           #:reduce-elements
           #:reduce-nodes
           #:reduce-key-value
           
           biggest-item                  ; sorted-container-mixin
           smallest-item                 ; sorted-container-mixin
           
           first-item
           first-element
           last-item
           last-element
           current-item
           
           element-type                  ; typed-container-mixin
           initial-element
           initial-element-fn
           initial-contents
           
           delete-first
           delete-last
           
           append-item
           append-new-item
           
           delete-biggest-item           ; heap-container
           k-best-number                 ; k-best-heap-container
           
           make-container
           print-container
           
           remove-items-if
           count-elements
           count-elements-if
           
           count-using
           collect-using
           
           iterate-elements-stably
           iterate-key-value-stably
           collect-elements-stably
           #:collect-key-value-stably
           
           make-node-for-container
           
           ;; Specialized container operations
           enqueue                       ; queues
           dequeue
           
           pop-item                      ; stacks
           push-item
           
           insert-item-at                ; vector-containers
           delete-item-at
           
           inorder-walk                  ; Binary-Search-Trees
           preorder-walk
           postorder-walk
           
           insert-item-after             ; dlist-container
           insert-item-before
           delete-item-after
           delete-item-before
           replace-item
           next-item
           previous-item
           
           initial-element               ; Associative containers
           item-at
           item-at-1
           item-at!
           item-at-1!
           dimensions
           iterate-key-value
           some-item-p                  ;  Methods should be added for 
           every-item-p                 ;   more general containers
           some-element-p
           every-element-p
           some-key-value-p
           every-key-value-p
           reverse-find
           find-value
           find-key-value
           collect-keys
           iterate-keys
           
           element                       ; nodes
           parent
           children
           root
           contents
           find-child-node
           
           notify-element-of-child-status ; quad-tree
           
           ensure-sorted ; sorted-list-container
           force-sort
           update-item
           
           search-for-matching-node
           
           #:sample-element
           #:sample-item
           #:sample-elements
           #:sample-unique-elements
           #:safe-sample-unique-elements
           #:sort-elements
           #:sort-keys
           
           #:iterate-container-about-node
           #:insert-item-ordered-about-node
           #:insert-item-ordered
           #:left-node-for-item
           #:right-node-for-item
           #:left-and-right-nodes-for-item
           #:iterate-left-nodes
           #:iterate-right-nodes
           #:iterate-left
           #:iterate-right
           
           #:add-index
           #:delete-index
           #:insert-record
           #:delete-record
           #:update-record
           #:update-index
           
           #:table-named
           #:lookup-record
           
           #:*container-print-representation*
           #:*container-print-length*
           #:*container-print-association-string*
           #:*container-print-association-separator*
           
           #:nth-element
           #:nth-item
           #:best-item
           #:argmin
           #:argmax
           #:best-node
           #:best-element
           
           #:iterate-value-key
           
           #:map-containers
           #:collect-containers
           #:element-position
           
           #:key-exists-p)
  
  (:export #:current-element
           #:current-element-p
           #:element
           #:move-p
           #:move
           #:move-forward
           #:move-forward-p
           #:make-iterator
           #:make-generator
           #:iterate-with
           #:forward-iterator
           #:unique-value-iterator-mixin
           #:filtered-iterator-mixin
           #:filter
           #:reverse-container
           #:finish
           #:with-iterator
           #:iterate-forward
           
           #:weight
           #:unique-elements
           #:unique-nodes
           
           #:map-window-over-elements
           #:map-window-over-nodes
           #:collect-window-over-elements
           #:collect-window-over-nodes
           
           #:merge-elements
           #:merge-nodes
           #:element-counts
           #:node-counts
           
           #:map-pairs
           #:collect-pairs)
  
  ;; union find stuff, maybe not the best names...
  (:export #:representative
           #:find-set
           #:graft-nodes
           #:representative-node)
  
  (:export #:packages
           #:present-symbols-only-p
           #:exported-symbols-only-p)

  (:export
   #:existing-subclass
   #:include-class-dependencies
   #:add-parameter->dynamic-class
   #:determine-dynamic-class
   ;;??
   #+(or)
   #:add-dynamic-class-for-parameters
   #:remove-parameter->dynamic-class
   #:empty-add-parameter->dynamic-class
   #:empty-all-add-parameter->dynamic-class
   #:parameter->dynamic-class
   #:find-existing-subclass
   #:find-or-create-class)
  )
