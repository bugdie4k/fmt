(asdf:defsystem :fmt
  :description "Debug printing improved"
  :version "0.0.0"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :serial t
  :depends-on ("alexandria")
  :components ((:file "package")
               (:file "utils")
               (:file "fmt")               
               (:file "dbp")))