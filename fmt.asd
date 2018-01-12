(asdf:defsystem :fmt
  :description "Debug printing improved"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :homepage "https://github.com/bugdie4k/fmt"
  :version "0.0.1"
  :depends-on (:alexandria)  
  :components ((:module "src"
                :serial t
                :components ((:file "package")
                             (:file "utils")
                             (:file "fmt")
                             (:file "dbp-tokens")
                             (:file "dbp-keywords")
                             (:file "dbp"))))
  :in-order-to ((asdf:test-op (asdf:test-op :fmt-test))))
