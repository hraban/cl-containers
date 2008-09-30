(in-package #:containers)

(defclass* file-backed-table-container (table-container)
  ((pathspec nil ir)
   (size 0 a)
   (require-write (make-container 'list-container) r)
   )
  (:documentation "This is a very half-assed class that pretends to store a
table on disk. It doesn't support updating.")
  (:export-p t))


(defmethod save-row (column-names row out)
  (format out "~&\(~{~S ~}\)" 
          (collect-elements 
           column-names
           :transform (lambda (name)
                        (slot-value row name)))))

  
(defmethod save-table-header ((container file-backed-table-container) 
                              stream)
  (format stream "~&\(:prototype-name ~S :columns ~S\)"
          (class-name (prototype container))
          (column-names container)))


(defmethod load-table-header ((container file-backed-table-container) 
                              line)
  ;;?? STUB
  (values t))


(defmethod check-table-header ((container file-backed-table-container) 
                               line)
  ;;?? STUB
  (values t))


(defmethod save-changes ((container file-backed-table-container)) 
  (let ((column-names (column-names container))
        (write-header? nil))
    (if (probe-file (pathspec container))
      (check-table-header container (pathspec container))
      (setf write-header? t))
    
    (with-open-file (out (pathspec container)
                         :if-exists :append
                         :direction :output
                         :if-does-not-exist :create)
      (let ((*print-right-margin* most-positive-fixnum))
        (when write-header?
          (save-table-header container out))
        (iterate-elements
         (require-write container)
         (lambda (row)
           (save-row column-names row out))))))
  (empty! (require-write container)))


(defmethod insert-record ((table file-backed-table-container) object)
  (insert-item (require-write table) object)
  (incf (size table))
  (maybe-save-changes table))


(defmethod maybe-save-changes ((table file-backed-table-container))
  (when (> (size (require-write table)) 10)
    (save-changes table)))


(defmethod iterate-container ((container file-backed-table-container) fn)
  (save-changes container)
  (let ((first? t)
        (column-names (column-names container)))
    (u:map-lines-in-file 
     (lambda (line)
       (if first?
         ;; skip header row
         (setf first? nil)
       
         ;; create element and call fn on it
         (funcall fn (create-record container column-names line))))
     (pathspec container))))


(defmethod create-record ((container file-backed-table-container) 
                          (column-names list) (line string))
  (let ((it (allocate-instance (prototype container))))
    (loop for datum in (read-from-string line)
          for column in column-names do
          (setf (slot-value it column) datum))
    it))