(asdf:defsystem :fmt-test
  :description "Test fmt with lisp-unit"
  :author "Danylo Fedorov <fedorough@gmail.com>"
  :depends-on (:fmt :lisp-unit)
  :components ((:file "test"))
  :perform (asdf:test-op (o s) (uiop:symbol-call :fmt-test :test-all)))
