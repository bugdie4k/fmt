(in-package #:cl-user)

(defpackage #:fmt-test
  (:nicknames #:f-tst)
  (:use #:cl #:fmt)
  (:import-from #:lisp-unit
                #:define-test
                #:assert-true
                #:run-tests
                #:run-tags
                #:*print-failures*
                #:*print-errors*
                #:*print-summary*
                #:*summarize-results*
                #:print-errors
                #:print-failures)
  (:export #:test-fmt
           #:test-dbp
           #:test-all))

(in-package #:fmt-test)

(defun fmt.0 ()
  (let ((*standard-output* (make-string-output-stream)))
    (fmt "~ ~~ :: :7@A" :heh)
    (get-output-stream-string *standard-output*)))

(defparameter *fmt-common-control-string* ":{~test:::5@A-:S-:4R-:{:A:^+:}:}")
(defparameter *fmt-common-format-arg*
  (list (* (+ 123 2) 2) 2 5 (list :a "b" 'c :d)))

(defparameter *fmt-expected-1* "~test:  250-2-11-A+b+C+D")

(defun fmt.1 ()
  (let ((*standard-output* (make-string-output-stream)))
    (fmt *fmt-common-control-string* *fmt-common-format-arg*)
    (get-output-stream-string *standard-output*)))

(defun fmt.2 ()
  (with-output-to-string (s)
    (fmt :s s *fmt-common-control-string* *fmt-common-format-arg*)))

(defun fmt.3 ()
  (fmt :s nil *fmt-common-control-string* *fmt-common-format-arg*))

(defparameter *fmt-expected-2*
  (concatenate 'string *fmt-expected-1* (list #\newline)))

(defun fmt.4 ()
  (fmt :nl :s nil *fmt-common-control-string* *fmt-common-format-arg*))

(defun fmt.5 ()
  (fmt :s nil :nl *fmt-common-control-string* *fmt-common-format-arg*))

(define-test fmt
  (:tag :fmt)
  (assert-true (string= "~ ~~ :     HEH" (fmt.0)))
  (assert-true (string= *fmt-expected-1* (fmt.1)))
  (assert-true (string= *fmt-expected-1* (fmt.2)))
  (assert-true (string= *fmt-expected-1* (fmt.3)))
  (assert-true (string= *fmt-expected-2* (fmt.4)))
  (assert-true (string= *fmt-expected-2* (fmt.5))))

(defparameter *fmt4l-common-control-string* ":A>")
(defparameter *fmt4l-common-format-args*
  (list 1 2 :a (list 'quote (coerce "opana" 'list)) 5))

(defparameter *fmt4l-expected-1* "1>2>A>(o p a n a)>5>")

(defmacro fmt4l.1 ()
  `(let ((*standard-output* (make-string-output-stream)))
     (fmt4l ,*fmt4l-common-control-string*
            ,@*fmt4l-common-format-args*)
     (get-output-stream-string *standard-output*)))

(defmacro fmt4l.2 ()
  `(with-output-to-string (s)
     (fmt4l :s s ,*fmt4l-common-control-string*
            ,@*fmt4l-common-format-args*)))

(defmacro fmt4l.3 ()
  `(fmt4l :s nil ,*fmt4l-common-control-string*
          ,@*fmt4l-common-format-args*))

(defparameter *fmt4l-expected-2* "1>==2>==A>==(o p a n a)>==5>")

(defmacro fmt4l.4 ()
  `(fmt4l :s nil :d "=="
          ,*fmt4l-common-control-string*
          ,@*fmt4l-common-format-args*))

(defmacro fmt4l.5 ()
  `(fmt4l :d "==" :s nil
          ,*fmt4l-common-control-string*
          ,@*fmt4l-common-format-args*))

(defparameter *fmt4l-expected-3* "1>==2>==A>==(o p a n a)>==5>==")

(defmacro fmt4l.6 ()
  `(fmt4l :d+ "==" :s nil
          ,*fmt4l-common-control-string*
          ,@*fmt4l-common-format-args*))

(defparameter *fmt4l-expected-4*
  (concatenate 'string *fmt4l-expected-3* (list #\newline)))

(defmacro fmt4l.7 ()
  `(fmt4l :s nil :d+ "==" :nl
          ,*fmt4l-common-control-string*
          ,@*fmt4l-common-format-args*))

(define-test fmt4l
  (:tag :fmt :fmt4l)
  (assert-true (string= *fmt4l-expected-1* (fmt4l.1)))
  (assert-true (string= *fmt4l-expected-1* (fmt4l.2)))
  (assert-true (string= *fmt4l-expected-1* (fmt4l.3)))
  (assert-true (string= *fmt4l-expected-2* (fmt4l.4)))
  (assert-true (string= *fmt4l-expected-2* (fmt4l.5)))
  (assert-true (string= *fmt4l-expected-3* (fmt4l.6)))
  (assert-true (string= *fmt4l-expected-4* (fmt4l.7))))

(defparameter *fmts-common-control-string* ":A")

(defparameter *fmts-expected-1* "1<br>2")

(defun fmts.1 ()
  (fmts :d "<br>" :s nil
        (*fmts-common-control-string* 1)
        (*fmts-common-control-string* 2)))

(defun fmts.2 ()
  (let ((*standard-output* (make-string-output-stream)))
    (fmts :d "<br>"
          (*fmts-common-control-string* 1)
          (*fmts-common-control-string* 2))
    (get-output-stream-string *standard-output*)))

(defun fmts.3 ()
  (with-output-to-string (s)
    (fmts :d "<br>" :s s
          (*fmts-common-control-string* 1)
          (*fmts-common-control-string* 2))))

(defparameter *fmts-expected-2* (concatenate 'string *fmts-expected-1* "<br>"))

(defun fmts.4 ()
  (fmts :s nil :d+ "<br>"
        (*fmts-common-control-string* 1)
        (*fmts-common-control-string* 2)))

(defun fmts.5 ()
  (with-output-to-string (s)
     (fmts :d+ "<br>" :s s
           (*fmts-common-control-string* 1)
           (*fmts-common-control-string* 2))))

(defparameter *fmts-expected-3*
  "1
- 2
- 1>2>A>(o p a n a)>5>
- 1
-- 2
-- 1>2>A>(o p a n a)>5>
-- 1
--- 2
--- 1>2>A>(o p a n a)>5>")

;; obscure features
(defmacro fmts.6 ()
  `(fmts :s nil :d ":%- "
         (,*fmts-common-control-string* 1)
         (,*fmts-common-control-string* 2)
         (:l ,*fmt4l-common-control-string*
             ,@*fmt4l-common-format-args*)
         (:s :d ":%-- "
             (,*fmts-common-control-string* 1)
             (,*fmts-common-control-string* 2)
             (:l ,*fmt4l-common-control-string*
                 ,@*fmt4l-common-format-args*)
             (:s :d ":%--- "
                 (,*fmts-common-control-string* 1)
                 (,*fmts-common-control-string* 2)
                 (:l ,*fmt4l-common-control-string*
                     ,@*fmt4l-common-format-args*)))))

(define-test fmts
  (:tag :fmt :fmts)
  (assert-true (string= *fmts-expected-1* (fmts.1)))
  (assert-true (string= *fmts-expected-1* (fmts.2)))
  (assert-true (string= *fmts-expected-1* (fmts.3)))
  (assert-true (string= *fmts-expected-2* (fmts.4)))
  (assert-true (string= *fmts-expected-2* (fmts.5)))
  (assert-true (string= *fmts-expected-3* (fmts.6))))

(defparameter *dbp-expected-1*
  (format nil 
          "┌ 0   1 3      2~%~
           └ 0   1 3      5~%"))

(defun dbp.1 ()
  (let ((*standard-output* (make-string-output-stream)))
    (dbp :p> 1 :m> 2 :p> 3 :m> nl 5 ?rsc)
    (get-output-stream-string *standard-output*)))

(defun dbp.2 ()
  (dbp :?s nil :p> 1 :m> 2 :p> 3 :m> nl 5 ?rsc))

(define-test dbp
  (:tag :dbp)
  (assert-true (string= *dbp-expected-1* (dbp.1)))
  (assert-true (string= *dbp-expected-1* (dbp.2))))

(defmacro test (&rest run)
  `(let* ((*print-failures* t)
          (*print-errors* t)
          (*print-summary* t)
          (*summarize-results* t)
          (test-results (,@run (find-package :fmt-test))))
     (print-errors test-results)
     (print-failures test-results)))

(defun test-fmt ()
  (test run-tags '(:fmt)))

(defun test-dbp ()
  (test run-tags '(:dbp)))

(defun test-all ()
  (test run-tests :all))
