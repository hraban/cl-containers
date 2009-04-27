#|

Author: Gary King, et. al.  

Major contributions by Andrew Hannon, David Westbrook, Gary King, 
Brent Heeringa, Louis Theran

|#

(defpackage #:containers-system (:use #:cl #:asdf))
(in-package #:containers-system)

;; try hard
(unless (find-system 'asdf-system-connections nil)
  (warn "The CL-Containers system would enjoy having ~
asdf-system-connections around. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))
(when (find-system 'asdf-system-connections nil)
  (operate 'load-op 'asdf-system-connections))

(defsystem cl-containers
  :version "0.11.5"
  :author "Brendan Burns, Andrew Hannon, Brent Heeringa, Gary King, Joshua Moody, Charles Sutton, Louis Theran, David Westbrook, and other former students and staff of EKSL."
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"
  :components 
  ((:module 
    "dev"
    :components
    ((:file "package")
     (:file "conditions"
	    :depends-on ("package"))
     (:file "container-api" 
	    :depends-on ("conditions" "package"))
     (:file "containers"
	    :depends-on ("conditions" "package"))
     (:file "basic-operations"
	    :depends-on ("container-api" "containers"))
     (:file "queues"
	    :depends-on ("basic-operations"))
     (:file "stacks"
	    :depends-on ("package" "basic-operations"))
     (:file "trees"
	    :depends-on ("package" "basic-operations" "vectors"))
     (:file "lists"
	    :depends-on ("package" "basic-operations"))
     (:file "bags-and-sets"
	    :depends-on ("package" "basic-operations"))
     (:file "ring-buffers"
	    :depends-on ("package" "basic-operations" "queues"))
     (:file "miscellaneous"
	    :depends-on ("package" "basic-operations"))
     (:file "associative"
	    :depends-on ("package" "basic-operations"))
     (:file "compatibility" 
	    :depends-on ("package" "basic-operations" "associative"))
     (:file "vectors"
	    :depends-on ("package" "basic-operations"))
     (:file "quad-tree"
	    :depends-on ("package" "basic-operations" "trees"))
     (:file "heaps"
	    :depends-on ("package" "basic-operations"))
     (:file "container-mixins"
	    :depends-on ("package" "basic-operations"))
     (:file "union-find-container"
	    :depends-on ("package" "basic-operations"))
     (:file "package-container"
	    :depends-on ("package" "basic-operations"))
     (:file "iterator-api"
	    :depends-on ("package"))
     (:file "iterators"
	    :depends-on ("iterator-api" "basic-operations"))
     (:file "file-iterators"
	    :depends-on ("iterators"))
     #+(or digitool openmcl)
     (:file "containers-readtable"
	    :depends-on ("containers"))))
   (:module "website"
	    :components ((:module "source"
				  :components ((:static-file "index.md"))))))
  :in-order-to ((test-op (load-op cl-containers-test)))
  :perform (test-op :after (op c)
                    (describe 
		     (funcall (intern (symbol-name '#:run-tests) :lift) 
			      :config :generic)))
  :depends-on ((:version :metatilities-base "0.6.6")))

(defmethod operation-done-p 
           ((o test-op) (c (eql (find-system 'cl-containers))))
  (values nil))


#+asdf-system-connections
(asdf:defsystem-connection containers-moptilities
  :requires (cl-containers moptilities)
  :components ((:module "dev"
                        :components ((:file "containers-moptilities")))))

#+asdf-system-connections
(asdf:defsystem-connection containers-and-utilities
  :requires (cl-containers metatilities-base)
  :components ((:module "dev"
                        :components ((:file "utilities-integration")))))

#+asdf-system-connections
(asdf:defsystem-connection containers-and-variates
  :requires (cl-containers cl-variates)
  :components ((:module 
		"dev"
		:components ((:file "container-sampling")
			     (:file "weighted-sampling"
				    :depends-on ("container-sampling"))))))

#+(and (not sbcl) asdf-system-connections)
(asdf:defsystem-connection containers-and-metacopy
  :requires (cl-containers metacopy)
  :components ((:module "dev"
                        :components ((:file "copying")))))


#+asdf-system-connections
(asdf:defsystem-connection containers-and-dynamic-classes
  :requires (cl-containers dynamic-classes)
  :components ((:module 
		"dev"
		:components ((:file "dynamic-classes")))))


#-asdf-system-connections
(defsystem container-dynamic-classes
  :depends-on (:cl-containers :dynamic-classes)
  :components ((:module 
		"dev"
		:components ((:file "dynamic-classes")))))


#|

(define-eksl-module :container-immutable 
  ("immutable-containers" :depends-on ("conditions"))
  :system containers)

(define-eksl-module :container-tables 
  ((("table-container"
     "file-backed-table-container")))
  :system containers
  :depends-on (containers moptilities))

(define-eksl-module :r-tree
  ("r-tree")
  :system :containers)

(define-eksl-module :container-thread-safe 
  ((("container-thread-safe")))
  :system containers
  :depends-on (containers))

|#   
