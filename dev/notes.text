(in-package containers)

what the hell happened to eksl-priority-queue?

I wonder if something like (collect-elements ... :sort ...) would be handy
  it might be faster than (sort (collect-elements ...) ...)

Remove / deprecate:
make-array-container
ok? - make-heap-container
ok? - make-k-best-heap-container
make-ring-buffer
make-sorted-queue

can node-empty-p be empty-p instead?


copying

(define-debugging-class debug-container ()
  ())

:make-load-form
  abstract-container
  bst-node
  quad-tree-node
  priority-queue-heap

(define-debugging-class debug-quad-tree (debug-container)
  ())

ok - awhen
ok make-ht-for-container ==> make-container-for-contents
ok - make-bst-node; removed, not called
ok - make-quad-tree-node, only called by make-node-for-container
ok - make-heap-node, only called by make-node-for-container
