(in-package #:containers)

(defclass* filtered-container-mixin ()
  ((element-filter nil ia)
   (key-filter nil ia))
  (:export-slots element-filter))


(defmethod iterate-elements :around ((container filtered-container-mixin) fn)
  (let ((filter (element-filter container)))
    (if filter
      (call-next-method container 
                        (lambda (element)
                          (when (funcall filter element)
                            (funcall fn element))))
      (call-next-method container fn))))


(defmethod collect-elements :around ((container filtered-container-mixin)
                                     &key filter transform)
  (let ((element-filter (element-filter container)))
    (if element-filter
      (call-next-method container 
                        :filter (lambda (element)
                                  (when (funcall element-filter element)
                                    (or (not filter) (funcall filter element))))
                        :transform transform)
      (call-next-method container))))


(defmethod iterate-key-value :around ((container filtered-container-mixin) fn)
  (let ((element-filter (element-filter container))
        (key-filter (key-filter container)))
    (if (or element-filter key-filter)
      (call-next-method 
       container
       (lambda (k v)
         (when (and (or (not element-filter)
                        (and element-filter (funcall element-filter v)))
                    (or (not key-filter)
                        (and key-filter (funcall key-filter k))))
           (funcall fn k v))))
      (call-next-method))))

