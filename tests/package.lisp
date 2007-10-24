(in-package #:common-lisp-user)

(defpackage #:metabang.cl-containers.test
  (:use #:common-lisp #:cl-containers #:lift)
  (:nicknames #:cl-containers-test)
  (:import-from #:metatilities
                #:deprecated
                #:defclass*
                #:defcondition
                #:ensure-list
                #:find-or-create-class
                #:length-at-most-p 
                #:length-at-least-p
                #:length-1-list-p
                #:maparray
                #:reset
                #:samep
                #:set-equal

                #:add-parameter->dynamic-class
                #:add-dynamic-class-for-parameters
                #:determine-dynamic-class
                #:existing-subclass
                #:find-existing-subclass
                #:include-class-dependencies

                #:*samep-test*
                #:parent
                #:element
                #:argmax
                #:argmin
                #:best-item
                
                #:size
                #:root))