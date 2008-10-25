(in-package #:containers)

(defgeneric make-iterator 
    (iteratee &rest args &key iterator-class &allow-other-keys)
  (:documentation ""))

(defgeneric print-iterator (iterator stream)
  (:documentation ""))

(defgeneric finish (iterator)
  (:documentation "Tell Lisp that you are done with this iterator. Further calls to current-element, etc. will have unspecified behavior and may cause an error."))


(defgeneric move-p (iterator direction)
  (:documentation ""))


(defgeneric element-passes-p (iterator)
  (:documentation "")
  (:method-combination and))


(defgeneric move (iterator direction)
  (:documentation ""))


(defgeneric base-class-for-iteratee (container)
  (:documentation ""))


(defgeneric class-for-contents-as (contents as)
  (:documentation ""))


(defgeneric current-element (thing)
  (:documentation ""))


(defgeneric current-element-p (iterator)
  (:documentation ""))


(defgeneric setup-initial-container (object)
  (:documentation ""))


(defgeneric advance (iterator)
  (:documentation ""))


(defgeneric iterate-forward (iterator function)
  (:documentation ""))


(defgeneric make-internal-iterator (object container)
  (:documentation ""))

(defgeneric move-forward (iterator)
  (:documentation ""))

(defgeneric move-forward-to-next-element (iterator)
  (:documentation ""))

(defgeneric move-internal (iterator direction)
  (:documentation ""))

(defgeneric next-element (iterator)
  (:documentation ""))

(defgeneric open-file-for-iterator (object filename)
  (:documentation ""))

(defgeneric reset (iterator)
  (:documentation ""))