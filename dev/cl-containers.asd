#|

$Id: containers.asdf,v 1.1 2004/07/22 01:04:32 gwking Exp $

Copyright 199?-2002 Experimental Knowledge Systems Lab, University of Massachusetts Amherst
Professor Paul Cohen, Director

Author: Gary King, et. al.  
most recent working over by Andrew Hannon, David Westbrook, Gary King, 
Brent Heeringa, Louis Theran)

|#

(defpackage :containers-system (:use #:cl #:asdf))
(in-package :containers-system)

;;; ---------------------------------------------------------------------------
;;; system definitions
;;; ---------------------------------------------------------------------------

(defsystem CL-CONTAINERS
  :version "0.8"
  :author "Gary King <gwking@cs.umass.edu>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "A generic container library for Common Lisp"
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
               (:file "containers-readtable"
                      :depends-on ("containers")))
  :depends-on (MOPTILITIES
               METATILITIES-BASE))

#|

(define-eksl-module :container-immutable 
  ("immutable-containers")
  :system containers)

;;; ---------------------------------------------------------------------------

(defsystem-connection containers-and-utilities
  ((("dynamic-class") :base-dir "eksl-utils:source;")

   (("iterators"))
   
   (("utilities-integration"
     "eksl-priority-queue")))
  :bin-identifiers (:platform :vendor)
  :requires (containers :eksl-utilities-base-1))

;;; ---------------------------------------------------------------------------

(defsystem-connection containers-and-copying
  ("container-copying")

  :bin-identifiers (:platform :vendor)
  :requires (containers copying))

;;; ---------------------------------------------------------------------------

(defsystem-connection containers-and-variates
  ("container-sampling"
   "weighted-sampling")

  :bin-identifiers (:platform :vendor)
  :requires (containers variates))

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
