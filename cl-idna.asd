(defsystem "cl-idna"
  :version "0.1.0"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("alexandria"
               "cl-unicode")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "CL-IDNA is a Internationalized Domain Names in Applications API"
  :in-order-to ((test-op (test-op "cl-idna/tests"))))

(defsystem "cl-idna/tests"
  :author "Nikolai Matiushev"
  :license "MIT"
  :depends-on ("cl-idna"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-idna"
  :perform (test-op (op c) (symbol-call :rove :run c)))
