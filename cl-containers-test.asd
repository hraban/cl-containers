(in-package common-lisp-user)

(defpackage #:cl-containers-test-system (:use #:common-lisp #:asdf))
(in-package #:cl-containers-test-system)

(defsystem cl-containers-test
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for CL-Containers"
  :components ((:module "test"
                        :components ((static-file "notes.text")
                                     
                                     (:file "package")
				     (:file "test-containers" 
				     	    :depends-on ("package"))
				     (:file "misc" 
				     	    :depends-on ("test-containers"))
                                     #+Ignore
                                     (:file "test-*" 
                                            :depends-on ("package")))))
  :depends-on (cl-containers lift))