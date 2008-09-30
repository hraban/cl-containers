
(in-package #:containers)

(defclass* immutable-keys-mixin ()
  "Keys may not be added or changed after the container is created."
  ((allow-mutation? nil ia))
  (:export-p t)
  :export-slots)


(defmethod item-at! :around ((container immutable-keys-mixin) 
                             value &rest indexes)
  (declare (dynamic-extent indexes))
  (when (and (completely-created? container)
             (not (allow-mutation? container)))
    (unless (key-exists-p container indexes)
      (error 'key-does-not-exist-error)))
  (call-next-method)
  value)


#|

(lift:deftestsuite test-immutable-keys-mixin ()
  ())

(lift:addtest (test-immutable-keys-mixin)
  foo
  (let ((c (make-container '(immutable-keys-mixin alist-container))))
    (lift:ensure-error (setf (item-at c :a) 2))))

(lift:addtest (test-immutable-keys-mixin)
  bar
  (let ((c (make-container '(alist-container))))
    (lift:ensure-error (setf (item-at c :a) 2))))
|#