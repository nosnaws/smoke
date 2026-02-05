(defsystem "smoke-test"
  :author "Alec Swanson"
  :license "MIT"
  :description "Tests for smoke"
  :depends-on ("smoke" "fiveam" "str")
  :serial t
  :components ((:module "t"
                :components ((:file "package")
                             (:file "state-test"))))
  :perform (test-op (op c)
             (symbol-call :fiveam :run! :smoke-tests)))
