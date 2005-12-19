#|

Author: Gary King, et. al.  
most recent working over by Andrew Hannon, David Westbrook, Gary King, 
Brent Heeringa, Louis Theran)

|#

(defpackage "CONTAINERS-SYSTEM" (:use #:cl #:asdf))
(in-package "CONTAINERS-SYSTEM")

;;; ---------------------------------------------------------------------------
;;; system definitions
;;; ---------------------------------------------------------------------------

;; try hard
(unless (find-system 'asdf-system-connections nil)
 (when (find-package 'asdf-install)
   (funcall (intern "INSTALL" "ASDF-INSTALL") 'asdf-system-connections)))
;; give up with a useful (?) error message
(unless (find-system 'asdf-system-connections nil)
  (error "The CL-Containers system requires ASDF-SYSTEM-CONNECTIONS. See 
http://www.cliki.net/asdf-system-connections for details and download
instructions."))

;;; ---------------------------------------------------------------------------

(defsystem cl-containers
  :version "0.8"
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"
  :components ((:module "dev"
                        :components ((:file "package")
                                     (:file "container-api" 
                                            :depends-on ("package"))
                                     (:file "containers"
                                            :depends-on ("package"))
                                     (:file "basic-operations"
                                            :depends-on ("package" "containers"))
                                     (:file "queues"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "stacks"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "trees"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "lists"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "bags-and-sets"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "ring-buffers"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "miscellaneous"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "associative"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "compatibility" 
                                            :depends-on ("package" "basic-operations" "associative"))
                                     (:file "vectors"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "quad-tree"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "heaps"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "container-mixins"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "union-find-container"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "package-container"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "iterators"
                                            :depends-on ("package" "basic-operations"))
                                     (:file "containers-readtable"
                                            :depends-on ("containers")))))
  :depends-on (asdf-system-connections
               metatilities-base metabang-dynamic-classes))

;;; ---------------------------------------------------------------------------

(asdf:defsystem-connection containers-and-utilities
  :requires (cl-containers metatilities-base)
  :components ((:module "dev"
                        :components ((:file "utilities-integration")))))

;;; ---------------------------------------------------------------------------

#+NotYet
(asdf:defsystem-connection containers-and-variates
  :requires (cl-containers cl-variates)
  :components ((:module "dev"
                        :components ((:file "container-sampling")
                                     (:file "weighted-sampling")))))

#|

(define-eksl-module :container-immutable 
  ("immutable-containers")
  :system containers)

;;; ---------------------------------------------------------------------------

(defsystem-connection containers-and-copying
  ("container-copying")

  :bin-identifiers (:platform :vendor)
  :requires (containers copying))

;;; ---------------------------------------------------------------------------

(define-eksl-module :container-tables 
  ((("table-container"
     "file-backed-table-container")))
  :system containers
  :depends-on (containers moptilities))

;;; ---------------------------------------------------------------------------

(define-eksl-module :r-tree
  ("r-tree")
  :system :containers)

;;; ---------------------------------------------------------------------------

(define-eksl-module :container-thread-safe 
  ((("container-thread-safe")))
  :system containers
  :depends-on (containers))

;;; ---------------------------------------------------------------------------

(define-eksl-system :test-containers
  ((("test-*"
     
     ) :load-only? t))
  
  :bin-identifiers (:platform :vendor)
  :depends-on (lift containers)
  :nice-name "Containers Test Suite")

|#   

;;; ***************************************************************************
;;; *                              End of File                                *
;;; ***************************************************************************
