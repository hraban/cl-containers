(in-package #:common-lisp-user)

(defpackage #:cl-containers-test-system
  (:use #:common-lisp #:asdf))
(in-package #:cl-containers-test-system)

(defsystem cl-containers-test
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Tests for CL-Containers"
  :components ((:module "setup"
			:pathname "tests/" 
			:components ((:file "package")
				     (:file "test-containers" 
				      :depends-on ("package"))))
	       (:module "tests"
			:depends-on ("setup")
			:components 
			((static-file "notes.text")
			 (:file "priority-queues")
			 (:file "misc")
			 (:file "trees"))))
  :depends-on (:cl-containers :lift))
