(in-package #:cl-user)

(defpackage #:fmt
  (:nicknames #:f)
  (:use #:cl)
  (:import-from #:alexandria
                #:mappend
                #:curry
                #:hash-table-alist)
  (:export
   ;; src/fmt.lisp
   #:fmt
   #:fmt4l
   #:fmts
   #:format+
   #:format4l
   #:formats
   #:echo
   #:brk
   #:brk4l
   #:breacho
   ;; src/dbp.lisp
   #:dbp
   #:dbp-reset-counter))
