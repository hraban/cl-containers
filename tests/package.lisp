(in-package #:common-lisp-user)

(defpackage #:metabang.cl-containers.test
  (:use #:common-lisp #:cl-containers #:lift)
  (:nicknames #:cl-containers-test)
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
                #:root))