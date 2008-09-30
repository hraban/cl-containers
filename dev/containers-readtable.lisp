(in-package #:containers)

#|

#[basic-queue 3 4 5 6]
#[vector-container 4 1 2 3]
#[associative-container [4 5] [2 :a] [3 2]]

#[associative-container (4 5) ([1 2] :a) (3 2)]
(make-container 'associative-container
                :initial-contents '((4 3) 5 6 7))
(print-container *)
#[associative-container 4 5 1 :a 3 :c]

BB on DB with strong ontology
|#

(defvar *container-readtable* (copy-readtable))
(defvar *container-print-representation* :summary ;; :contents
  "Specifies the output format for container printing. This can be :contents or :summary.")
(defvar *container-print-length* nil
  "Specifies the maximum number of container elements to print when *container-print-representation* is :contents. If *container-print-length* less than the size of the container, then *container-print-length* elements will be printed followed by an ellipse. If *container-print-length* is null, then all elements will be printed.")
(defvar *container-print-association-string* " -> "
  "Specify the string used to separate keys and values when printing the contents of an associative-container.")
(defvar *container-print-association-separator* "; "
  "Specify the string used to separate <key value> pairs when printing the contents of an associative-container.")


(set-macro-character
   #\[
   #'(lambda (stream ignore)
       (declare (ignore ignore))
       (read-container-element stream #\]))
   nil *container-readtable*)

  
(set-macro-character 
 #\]
 #'(lambda (stream ch)
     (warn "Ignoring extra \"~c\" on ~s ." ch stream))
 nil *container-readtable*)


#+MCL
;;?? Gary King 2005-11-17: need to port
(defun read-container (stream &optional (termch #\]))
  (let* ((head (cons nil nil))
         (tail head))
    (declare (dynamic-extent head)
             (list head tail))
    (loop
      (multiple-value-bind (form form-p) 
                           (ccl::%read-list-expression stream nil termch)
        (when (not form-p) (return))
        (rplacd tail (setq tail (cons form nil)))))
    (cdr head)))


#+MCL
;;?? Gary King 2005-11-17: need to port
(defun read-container-element (stream &optional (termch #\]))
  (let* ((head (cons nil nil))
         (tail head))
    (declare (dynamic-extent head)
             (list head tail))
    (loop
      (multiple-value-bind (form form-p) 
                           (ccl::%read-list-expression stream nil termch)
        (when (not form-p) (return))
        (rplacd tail (setq tail (cons form nil)))))
    (cdr head)))


#+MCL
;;?? Gary King 2005-11-17: need to port
(set-dispatch-macro-character
 #\#
 #\[
 (lambda (stream subchar numarg)
  (declare (ignore subchar numarg))
  (let ((*readtable* *container-readtable*))
    (destructuring-bind (type &rest data) (ccl::read-list stream t #\])
      (make-container type :initial-contents data)))))


#+MCL
;;?? Gary King 2005-11-17: need to port
(defmethod print-object ((container abstract-container) stream)
  (ecase *container-print-representation*
    (:contents
     (format stream "#[~A" (type-of container))
     (print-container-contents container stream)
     (princ #\] stream))
    (:summary
     (print-unreadable-object (container stream :type t :identity t)
       (print-container-summary container stream)))))


(defmethod print-container-summary ((container abstract-container) stream)
  (format stream "~D" (size container)))


(defmethod print-container-contents ((container abstract-container) stream)
  (let* ((base-print-fn (lambda (element)
                          (princ " " stream)
                          (prin1 element stream))) 
         (print-fn 
          (if *container-print-length*
            (let ((count 0))
              (lambda (element)
                (when (> (incf count) *container-print-length*)
                  (princ " ..." stream)
                  (return-from print-container-contents))
                (funcall base-print-fn element)))
            base-print-fn)))
    (iterate-elements container print-fn)))


(defmethod print-container-contents ((container associative-container-mixin) stream)
  (let* ((first? t)
         (base-print-fn (lambda (key value)
                          (if first?
                            (princ " " stream)
                            (princ *container-print-association-separator* stream))
                          (setf first? nil)
                          (prin1 key stream)
                          (princ *container-print-association-string* stream)
                          (prin1 value stream))) 
         (print-fn 
          (if *container-print-length*
            (let ((count 0))
              (lambda (key value)
                (when (> (incf count) *container-print-length*)
                  (princ " ..." stream)
                  (return-from print-container-contents))
                (funcall base-print-fn key value)))
            base-print-fn)))
    (iterate-key-value container print-fn)))


(defmethod print-container-contents ((container array-container-abstract) stream)
  (princ "..." stream))


#+Test
(let ((*container-print-length* 5)
      (*container-print-representation* :contents)
      (c (make-container 'list-container)))
  (loop repeat 20 do
        (insert-item c (variates:integer-random variates:*random-generator* 0 100)))
  (print c)
  (values))

#+Test
(let ((*container-print-length* 5)
      (*container-print-representation* :contents)
      (c (make-container 'alist-container)))
  (loop for i from 1 to 20 do
        (setf (item-at c i)
              (variates:integer-random variates:*random-generator* 0 100)))
  (print c)
  (values))

;;; container-printing-mixin

(defclass* container-printing-mixin ()
  ((print-representation :unbound ia)
   (print-length :unbound ia)
   (print-association-string :unbound ia)
   (print-association-separator :unbound ia))
  (:default-initargs
    :print-representation :summary
    :print-length nil
    :print-association-string " -> "
    :print-association-separator ";"))


(defmethod print-object :around ((container container-printing-mixin) stream)
  (declare (ignorable stream))
  (let ((*container-print-representation* (print-representation container))
        (*container-print-length* (print-length container))
        (*container-print-association-string* (print-association-string container))
        (*container-print-association-separator* (print-association-separator container)))
    (call-next-method)))
        
#+Example
(let ((c (make-container '(simple-associative-container
                           container-printing-mixin)
                         :initial-contents '(:a 1 :b 2 :c 3 :d 4 :e 5)
                         :print-representation :summary)))
  (print c)
  (values))

#+Example
(let ((c (make-container '(simple-associative-container
                           container-printing-mixin)
                         :initial-contents '(:a 1 :b 2 :c 3 :d 4 :e 5)
                         :print-representation :contents
                         :print-length 3)))
  (print c)
  (values))