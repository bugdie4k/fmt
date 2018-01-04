(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-initial-keywords (args &rest keywords)
    (let ((keywords-ht (make-hash-table)))
      (labels ((%kw (kw-el)
                 (if (listp kw-el) (first kw-el) kw-el))
               (%num (kw-el)
                 (if (listp kw-el) (second kw-el) 0))
               (%default (kw-el)
                 (if (listp kw-el) (third kw-el) nil))
               (%find ()
                 (find-if (curry #'eq (first args)) keywords :key #'%kw))
               (%take (num)
                 (cond ((= num 0)
                        (prog1 (first args)
                          (setf args (rest args))))
                       ((= num 1)
                        (prog1 (second args)
                          (setf args (nthcdr 2 args))))
                       (t
                        (prog1 (subseq args 1 (1+ num))
                          (setf args (nthcdr (1+ num) args))))))
               (%try-parse-kw ()
                 (awhen (%find)
                   (setf (gethash (%kw it) keywords-ht)
                         (%take (%num it))))))
        (dotimes (i (length keywords))
          (unless (%try-parse-kw)
            (return)))
        (append (loop :for kw :in keywords
                   :collect
                     (multiple-value-bind (val set?)
                         (gethash (%kw kw) keywords-ht)
                       (if set? val (%default kw))))
                (list args)))))

  (defun fmt->format (fmt-string)
    (let ((fmt-string (if (stringp fmt-string)
                          fmt-string
                          (write-to-string fmt-string))))
      (concatenate 'string
                   (loop for ch across fmt-string
                      collecting (cond ((char-equal ch #\~) #\:)
                                       ((char-equal ch #\:) #\~)
                                       (t ch))))))

  (defun transform-fmt-str-form (fmt-str-form
                                 &key delimiter newline? translate?)
    (let* ((format-str-form (if translate?
                                `(fmt->format ,fmt-str-form)
                                fmt-str-form))
           (format-str-form (if (stringp fmt-str-form)
                                (eval format-str-form)
                                format-str-form))
           (actual-delimiter (when delimiter
                               (if translate?
                                   `(fmt->format ,delimiter)
                                   delimiter)))
           (actual-delimiter (if (stringp delimiter)
                                 (eval actual-delimiter)
                                 actual-delimiter))
           (format-str-form+d (if actual-delimiter
                                  `(concatenate 'string
                                                ,format-str-form
                                                ,actual-delimiter)
                                  format-str-form))
           (format-str-form+d (if newline?
                                  `(concatenate 'string ,format-str-form+d "~%")
                                  format-str-form+d)))
      (if (and (stringp format-str-form)
               (stringp actual-delimiter))
          (eval format-str-form+d)
          format-str-form+d)))

  (defun fmt-aux (stream newline fmt-str-form fmt-args &key (translate? t))
    `(format ,stream
             ,(transform-fmt-str-form fmt-str-form
                                      :newline? newline
                                      :translate? translate?)
             ,@fmt-args))

  (defun fmt4l-aux (stream newline delimiter delimiter+
                    fmt-str-form fmt-args &key (translate? t))
    (let* ((args-num (length fmt-args))
           (format-str-form-sym (gensym "FORMAT-STR-FORM"))
           (delimiter (or delimiter+ delimiter))
           (delimiter-sym (gensym "DELIMITER"))
           (stream-sym (gensym "STREAM"))
           (format-str-form
            `(let ((,format-str-form-sym ,(if translate?
                                              `(fmt->format ,fmt-str-form)
                                              fmt-str-form))
                   ,@(when delimiter
                       (list
                        `(,delimiter-sym ,(if translate?
                                              `(fmt->format ,delimiter)
                                              delimiter)))))
               (with-output-to-string (,stream-sym)
                 ,@(if (and delimiter (not delimiter+))
                       (list `(dotimes (i ,(1- args-num))
                                (princ ,format-str-form-sym
                                       ,stream-sym)
                                (princ ,delimiter-sym
                                       ,stream-sym))
                             `(princ ,format-str-form-sym
                                     ,stream-sym))
                       (list `(dotimes (i ,args-num)
                                (princ ,format-str-form-sym ,stream-sym)
                                ,@(when delimiter+
                                    (list `(princ ,delimiter-sym
                                                  ,stream-sym)))))))))
           (format-str-form
            (if newline
                `(concatenate 'string ,format-str-form "~%")
                format-str-form))
           (format-str-form (if (stringp fmt-str-form)
                                (eval format-str-form)
                                format-str-form)))
      `(format ,stream ,format-str-form ,@fmt-args)))

  (defun fmts-aux (stream delimiter delimiter+ fmt-lists &key (translate? t))
    (let* ((delimiter (or delimiter+ delimiter))
           (stream-sym (gensym "STREAM"))
           (stream-src (or stream '(make-string-output-stream))))
      (flet ((%make-format-call (fmt-list delimiter)
               `(format ,stream-sym
                        ,@(cons (transform-fmt-str-form (first fmt-list)
                                                        :delimiter delimiter
                                                        :translate? translate?)
                                (rest fmt-list)))))
        `(let ((,stream-sym ,stream-src))
           ,@(mapcar
              (lambda (fmt-list)
                 (%make-format-call fmt-list delimiter))
              (butlast fmt-lists))
           ,(%make-format-call (car (last fmt-lists)) delimiter+)
           ,(unless stream
              `(get-output-stream-string ,stream-sym)))))))

(defmacro fmt (&rest args)
  "SYNOPSIS
      (fmt [ :s stream ] [ :nl ] fmt-string fmt-args)
DESCRIPTION
      `fmt-string' is the same as control-string in format,
      but with ~ (tilde) and : (colon) swapped.
      You can omit stream argument - t is default.
      `:nl' adds newline in the end (same as :% would).
      Does the same as format - prints to stream."
  (destructuring-bind (stream newline fmt-string/args)
      (parse-initial-keywords args '(:s 1 t) :nl)
    (fmt-aux stream newline (first fmt-string/args) (rest fmt-string/args)
             :translate? t)))

(defmacro fmt4l (&rest args)
  "SYNOPSIS
       (fmt4l [ :s stream ] [ :nl ] [ [ :d | :d+ ] delimiter ]
              fmt-string fmt-args)
DESCRIPTION
       `fmt-string' is the same as control-string in format,
       but with ~ (tilde) and : (colon) swapped.
       Applies `fmt-string' to each arg in fmt-args.
       To put it simply, multiplies `fmt-string' by the number of arguments.
       `:nl' adds newline to the end.
       `:d' allows to set a delimiter to insert between elements.
       `:d+' does the same and also ends delimiter to the end."
  (destructuring-bind (stream newline delimiter delimiter+ fmt-string/args)
      (parse-initial-keywords args '(:s 1 t) :nl '(:d 1) '(:d+ 1))
    (fmt4l-aux stream newline delimiter delimiter+
               (first fmt-string/args) (rest fmt-string/args)
               :translate? t)))

(defmacro fmts (&rest args)
  "SYNOPSIS
      (fmts [ :s stream ] [ [ :d | :d+] delimiter ]
            (fmt-string fmt-args) ...)
DESCRIPTION
      `fmt-string' is the same as control-string in format,
      but with ~ (tilde) and : (colon) swapped.
      `fmts' is like `fmt' but you can specify several in one form.
      Default `stream' is t.
      `:d' allows to set a delimiter to insert between fmts.
       `:d+' does the same and also ends delimiter to the end."
  (destructuring-bind (stream delimiter delimiter+ fmt-lists)
      (parse-initial-keywords args '(:s 1 t) '(:d 1) '(:d+ 1))
    (fmts-aux stream delimiter delimiter+ fmt-lists
              :translate? t)))

(defmacro format2 (&rest args)
  "SYNOPSIS
      (format+ [ :s stream ] [ :nl ] format-string format-args)
DESCRIPTION
      You can omit stream argument - t is default.
      `:nl' adds newline in the end (same as :% would)."
  (destructuring-bind (stream newline format-string/args)
      (parse-initial-keywords args '(:s 1 t) :nl)
    (fmt-aux stream newline (first format-string/args) (rest format-string/args)
             :translate? t)))

(defmacro format4l (&rest args)
  "SYNOPSIS
       (format4l [ :s stream ] [ :nl ] [ [ :d | :d+ ] delimiter ]
                 format-string format-args)
DESCRIPTION
       Applies `format-string' to each arg in format-args.
       To put it simply, multiplies `format-string' by the number of arguments.
       `:nl' adds newline.
       `:d' allows to set a delimiter to insert between elements.
       `:d+' does the same and also ends delimiter to the end."
  (destructuring-bind (stream newline delimiter delimiter+ format-string/args)
      (parse-initial-keywords args '(:s 1 t) :nl '(:d 1) '(:d+ 1))
    (fmt4l-aux stream newline delimiter delimiter+
               (first format-string/args) (rest format-string/args)
               :translate? nil)))

(defmacro formats (&rest args)
  "SYNOPSIS
      (formats [ :s stream ] [ [ :d | :d+] delimiter ]
               (format-string format-args) ...)
DESCRIPTION
      Like `fmt' but you can specify several in one form.
      Default `stream' is t.
      `:d' allows to set a delimiter to insert between format statements.
       `:d+' does the same and also ends delimiter to the end."
  (destructuring-bind (stream delimiter delimiter+ format-lists)
      (parse-initial-keywords args '(:s 1 t) '(:d 1) '(:d+ 1))
    (fmts-aux stream delimiter delimiter+ format-lists
              :translate? t)))

(defmacro echo (&rest args)
  "SYNOPSIS
      (echo [ :-nl ] args)
DESCRIPTION
      Prints args with ~S formatting.
      You can pass `:-nl' as the first argument to avoid newline."
  (if (eq (first args) :-nl)
      `(fmt4l ":S " ,@(rest args))
      `(fmt4l :nl ":S " ,@args)))

;; TODO
;; (defmacro tbl (&rest args)
;;   )
