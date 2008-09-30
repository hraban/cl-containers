
(in-package #:containers)


(defgeneric copy-container (abstract-container)
  (:method ((container abstract-container))
           (metacopy:copy-thing container)))


(metacopy:defcopy-methods container-node-mixin :copy-all t)

(metacopy:defcopy-methods keyed-container-mixin :copy-all t)

(metacopy:defcopy-methods typed-container-mixin :copy-all t)

(metacopy:defcopy-methods initial-element-mixin :copy-all t)

(metacopy:defcopy-methods test-container-mixin :copy-all t)

(metacopy:defcopy-methods i-know-my-node-mixin :copy-all t)

(metacopy:defcopy-methods sorted-container-mixin :copy-all t)

(metacopy:defcopy-methods classified-container-mixin :copy-all t)

(metacopy:defcopy-methods uses-contents-mixin :copy-all t)

(metacopy:defcopy-methods priority-queue-on-container :copy-all t)

(metacopy:defcopy-methods stack-container :copy-all t)

(metacopy:defcopy-methods rooted-tree-container :copy-all t)

(metacopy:defcopy-methods parent-node-mixin :copy-all t)

(metacopy:defcopy-methods many-unordered-child-node :copy-all t)

(metacopy:defcopy-methods two-child-node :copy-all t)

(metacopy:defcopy-methods bst-node :copy-all t)

(metacopy:defcopy-methods quad-tree :copy-all t)

(metacopy:defcopy-methods four-child-node :copy-all t)

(metacopy:defcopy-methods quad-tree-node :copy-all t)

(metacopy:defcopy-methods basic-queue :copy-all t)

(metacopy:defcopy-methods bag-container :copy-all t)

(metacopy:defcopy-methods set-container :copy-all t)

(metacopy:defcopy-methods bag/set-container :copy-all t)

(metacopy:defcopy-methods keyed-bag/set-container :copy-all t)

(metacopy:defcopy-methods sorted-list-container :copy-all t)

(metacopy:defcopy-methods dlist-container-node :copy-all t)

(metacopy:defcopy-methods dlist-container :copy-all t)

(metacopy:defcopy-methods red-black-node :copy-all t)

(metacopy:defcopy-methods ring-buffer :copy-all t)

(metacopy:defcopy-methods array-container :copy-all t)

(metacopy:defcopy-methods sparse-array-container :copy-all t)

(metacopy:defcopy-methods alist-container :copy-all t)

(metacopy:defcopy-methods keyed-associative-container :copy-all t)

(metacopy:defcopy-methods heap-node :copy-all t)

(metacopy:defcopy-methods k-best-heap-container :copy-all t)

(metacopy:defcopy-methods stable-associative-container :copy-all t)

(metacopy:defcopy-methods associative-array :copy-all t)

(metacopy:defcopy-methods vector-container-mixin :copy-all t)

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************