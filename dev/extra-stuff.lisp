(defun determine-concrete-container-subclasses (root)
   (mapcar #'class-name
           (filter
            (lambda (class)
              (subtypep class 'concrete-container))
            (subclasses (find-class root)))))


