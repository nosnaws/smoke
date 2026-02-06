(defsystem "smoke"
  :author "Alec Swanson"
  :license "MIT"
  :version "0.1.4"
  :description "CLI for managing stacked diffs with squash-merge workflows"
  :depends-on ("alexandria"
               "serapeum"
               "str"
               "cl-json"
               "cl-ppcre"
               "uiop")
  :serial t
  :components ((:module "src"
                :components ((:file "package")
                             (:file "git")
                             (:file "state")
                             (:file "github")
                             (:file "commands")
                             (:file "main"))))
  :in-order-to ((test-op (test-op "smoke-test"))))
