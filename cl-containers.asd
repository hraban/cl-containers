#|

Author: Gary King, et. al.  

Major contributions by Andrew Hannon, David Westbrook, Gary King, 
Brent Heeringa, Louis Theran

|#

(defpackage #:containers-system (:use #:cl #:asdf))
(in-package #:containers-system)

;; try hard
(unless (or (member :asdf-system-connections *features*)
	    (find-system 'asdf-system-connections nil))
  (warn "The CL-Containers system would enjoy having ~
asdf-system-connections around. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))
(when (and (not (member :asdf-system-connections *features*))
	   (find-system 'asdf-system-connections nil))
  (operate 'load-op 'asdf-system-connections))

(defsystem cl-containers
  :version "0.12.1"
  :author "Brendan Burns, Andrew Hannon, Brent Heeringa, Gary King, Joshua Moody, Charles Sutton, Louis Theran, David Westbrook, and other former students and staff of EKSL."
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"
  :components 
  ((:module 
    "setup"
    :pathname "dev/"
    :components
    ((:file "package")
     (:file "conditions"
	    :depends-on ("package"))
     ))
   (:module 
    "dev"
    :depends-on ("setup")
    :components
    ((:file "container-api")
     (:file "containers")
     (:file "basic-operations"
	    :depends-on ("container-api" "containers"))
     (:file "queues"
	    :depends-on ("basic-operations"))
     (:file "stacks"
	    :depends-on ("basic-operations"))
     (:file "trees"
	    :depends-on ("basic-operations" "vectors"))
     (:file "lists"
	    :depends-on ("basic-operations"))
     (:file "bags-and-sets"
	    :depends-on ("basic-operations"))
     (:file "ring-buffers"
	    :depends-on ("basic-operations" "queues"))
     (:file "miscellaneous"
	    :depends-on ("basic-operations"))
     (:file "associative"
	    :depends-on ("basic-operations"))
     (:file "compatibility" 
	    :depends-on ("basic-operations" "associative"))
     (:file "vectors"
	    :depends-on ("basic-operations"))
     (:file "quad-tree"
	    :depends-on ("basic-operations" "trees"))
     (:file "heaps"
	    :depends-on ("basic-operations"))
     (:file "container-mixins"
	    :depends-on ("basic-operations"))
     (:file "union-find-container"
	    :depends-on ("basic-operations"))
     (:file "package-container"
	    :depends-on ("basic-operations"))
     (:file "iterator-api")
     (:file "iterators"
	    :depends-on ("iterator-api" "basic-operations"))
     (:file "file-iterators"
	    :depends-on ("iterators"))
     #+(or digitool openmcl)
     (:file "containers-readtable"
	    :depends-on ("containers"))
     (:file "dynamic-classes")
     (:file "dynamic-class-defs" :depends-on ("dynamic-classes" "containers"))))
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


