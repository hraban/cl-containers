(in-package #:cl-containers)

(defmethod mopu:when-finalized ((iterator basic-stream-iterator))
  ;;??
  ;(format t "~%GC: Maybe closing stream" iterator)
  (finish iterator))

