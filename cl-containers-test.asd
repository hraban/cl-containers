(defsystem "cl-containers-test"
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
                        ((:file "priority-queues")
                         (:file "trees")
                         (:file "dlist")
                         (:file "ring-buffers"))))
  :depends-on ("cl-containers"
               (:version "lift" "1.7.0"))
  :perform (test-op (op c)
             (describe
              (symbol-call :lift :run-tests :config :generic))))
