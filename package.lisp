(defpackage #:fmt
  (:nicknames #:f)
  (:use #:cl)
  (:import-from #:alexandria
                #:mappend
                #:switch
                #:symbolicate
                #:mappend
                #:curry
                #:hash-table-values
                #:hash-table-alist)
  (:export
   ;; fmt.lisp
   #:fmt
   #:fmt4l
   #:fmts
   #:format2
   #:format4l
   #:formats
   #:echo
   ;; dbp.lisp
   #:dbp   
   #:dbp-reset-counter))
