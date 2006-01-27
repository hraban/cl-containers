(in-package containers)

(defgeneric print-iterator (iterator stream)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric move-p (iterator direction)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric element-passes-p (iterator)
  (:documentation "")
  (:method-combination and))

;;; ---------------------------------------------------------------------------

(defgeneric move (iterator direction)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric base-class-for-iteratee (container)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric class-for-contents-as (contents as)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric current-element (thing)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric include-class-dependencies (class-type
                                        dynamic-class class-list &rest parameters)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric existing-subclass (class-type class-list)
  (:documentation ""))

;;; ---------------------------------------------------------------------------

(defgeneric setup-initial-container (object)
  (:documentation ""))

