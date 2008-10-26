(in-package #:common-lisp-user)

(defpackage #:metabang.cl-containers.documentation
  (:use #:common-lisp #:cl-containers #:cl-markdown-user
	#:metabang.docudown)
  (:nicknames #:cl-containers-documentation)
  (:import-from #:metatilities
                #:deprecated
                #:defclass*
                #:defcondition
                #:ensure-list
                #:length-at-most-p 
                #:length-at-least-p
                #:length-1-list-p
                #:maparray
                #:samep
                #:set-equal

                #:*samep-test*
                #:parent
                #:element
                #:argmax
                #:argmin
                #:best-item
                
                #:size
                #:root)

  (:import-from #:cl-containers
		#:basic-generator
		))