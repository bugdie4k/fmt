(asdf:defsystem :fmt
  :description "Debug print macros for Common Lisp"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :homepage "https://github.com/bugdie4k/fmt"
  :version "0.0.1"
  :depends-on (:alexandria)
  :serial t
  :components ((:file "package")
               (:module "src"
                :serial t
                :components ((:file "utils")
                             (:file "fmt")
                             (:file "dbp-tokens")
                             (:file "dbp-keywords")
                             (:file "dbp-parser")
                             (:file "dbp-translate-tokens")
                             (:file "dbp"))))
  :in-order-to ((asdf:test-op (asdf:test-op :fmt-test))))
