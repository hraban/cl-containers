(in-package #:containers)

(defclass* thread-safe-container-mixin ()
  ()
  (:export-p t))

(defmethod insert-item :around ((container thread-safe-container-mixin) item)
  (declare (ignore item))
  (u:with-write-access (:container)
    (call-next-method)))

(defmethod delete-first :around ((container thread-safe-container-mixin))
  (u:with-write-access (:container)
    (call-next-method)))

(defmethod iterate-nodes :around ((container thread-safe-container-mixin) fn)
  (declare (ignore fn))
  (u:with-read-access (:container)
    (call-next-method)))

(defmethod empty! :around ((container thread-safe-container-mixin))
  (u:with-write-access (:container)
    (call-next-method)))

(defmethod first-item :around ((container thread-safe-container-mixin))
  (u:with-read-access (:container)
    (call-next-method)))

(defmethod size :around ((container thread-safe-container-mixin))
  (u:with-read-access (:container)
    (call-next-method)))


