(in-package #:cl-user)

(defpackage #:fmt
  (:nicknames #:f)
  (:use #:cl)
  (:import-from #:alexandria
                #:mappend
                #:curry
                #:hash-table-values
                #:hash-table-alist
                #:alist-hash-table)  
  (:export
   ;; fmt.lisp
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
   ;; dbp.lisp
   #:dbp   
   #:dbp-reset-counter))
