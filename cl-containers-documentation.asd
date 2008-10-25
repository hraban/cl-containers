 (in-package #:common-lisp-user)

(defpackage #:cl-containers-documentation-system
  (:use #:common-lisp #:asdf))
(in-package #:cl-containers-documentation-system)

(defsystem cl-containers-documentation
  :author "Gary King <gwking@metabang.com>"
  :maintainer "Gary Warren King <gwking@metabang.com>"
  :licence "MIT Style License"
  :description "Documentation for CL-Containers"
  :components ((:module "setup"
			:pathname "docs/" 
			:components ((:file "package")
				     (:file "setup" 
				      :depends-on ("package"))))
	       (:module 
		"docs"
		:depends-on ("setup")
		:pathname "website/source/"
		:components
		((:docudown-source "index.md")
		 (:docudown-source "user-guide.md")
		 #+(or)
		 (:docudown-wild "shared:**:*.*"
				 :output "../output/shared/"))))
  :depends-on (:cl-containers :docudown))
