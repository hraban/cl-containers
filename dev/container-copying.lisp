
(in-package containers)

;;; ---------------------------------------------------------------------------

(defgeneric copy-container (abstract-container)
  (:method ((container abstract-container))
           (u:copy-top-level container)))

;;; ---------------------------------------------------------------------------

(u:defcopy-methods container-node-mixin :copy-all t)

(u:defcopy-methods keyed-container-mixin :copy-all t)

(u:defcopy-methods typed-container-mixin :copy-all t)

(u:defcopy-methods initial-element-mixin :copy-all t)

(u:defcopy-methods test-container-mixin :copy-all t)

(u:defcopy-methods i-know-my-node-mixin :copy-all t)

(u:defcopy-methods sorted-container-mixin :copy-all t)

(u:defcopy-methods classified-container-mixin :copy-all t)

(u:defcopy-methods uses-contents-mixin :copy-all t)

(u:defcopy-methods priority-queue-on-container :copy-all t)

(u:defcopy-methods stack-container :copy-all t)

(u:defcopy-methods rooted-tree-container :copy-all t)

(u:defcopy-methods parent-node-mixin :copy-all t)

(u:defcopy-methods many-unordered-child-node :copy-all t)

(u:defcopy-methods two-child-node :copy-all t)

(u:defcopy-methods bst-node :copy-all t)

(u:defcopy-methods quad-tree :copy-all t)

(u:defcopy-methods four-child-node :copy-all t)

(u:defcopy-methods quad-tree-node :copy-all t)

(u:defcopy-methods basic-queue :copy-all t)

(u:defcopy-methods bag-container :copy-all t)

(u:defcopy-methods set-container :copy-all t)

(u:defcopy-methods bag/set-container :copy-all t)

(u:defcopy-methods keyed-bag/set-container :copy-all t)

(u:defcopy-methods sorted-list-container :copy-all t)

(u:defcopy-methods dlist-container-node :copy-all t)

(u:defcopy-methods dlist-container :copy-all t)

(u:defcopy-methods red-black-node :copy-all t)

(u:defcopy-methods ring-buffer :copy-all t)

(u:defcopy-methods array-container :copy-all t)

(u:defcopy-methods sparse-array-container :copy-all t)

(u:defcopy-methods alist-container :copy-all t)

(u:defcopy-methods keyed-associative-container :copy-all t)

(u:defcopy-methods heap-node :copy-all t)

(u:defcopy-methods k-best-heap-container :copy-all t)

(u:defcopy-methods stable-associative-container :copy-all t)

(u:defcopy-methods associative-array :copy-all t)

(u:defcopy-methods vector-container-mixin :copy-all t)

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************