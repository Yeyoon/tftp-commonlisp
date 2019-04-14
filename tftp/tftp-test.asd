#|
  This file is a part of tftp project.
|#

(defsystem "tftp-test"
  :defsystem-depends-on ("prove-asdf")
  :author ""
  :license ""
  :depends-on ("tftp"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "tftp"))))
  :description "Test system for tftp"

  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
