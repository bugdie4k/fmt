(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun parse-initial-keywords (args &rest keywords)
    (let ((keywords-ht (make-hash-table)))
      (labels ((%keyword (keyword-el)
                 (if (listp keyword-el) (first keyword-el) keyword-el))
               (%num-to-take (keyword-el)
                 (if (listp keyword-el) (second keyword-el) 0))
               (%default-value (keyword-el)
                 (if (listp keyword-el) (third keyword-el) nil))
               (%find ()
                 (find-if (curry #'eq (first args)) keywords :key #'%keyword))
               (%take-next-args (num)
                 (cond ((= num 0)
                        (prog1 (first args)
                          (setf args (rest args))))
                       ((= num 1)
                        (prog1 (second args)
                          (setf args (nthcdr 2 args))))
                       ((> num 1)
                        (prog1 (subseq args 1 (1+ num))
                          (setf args (nthcdr (1+ num) args))))
                       (t (error "cannot take ~A (< 0) args" num))))
               (%try-parse-kw ()
                 (awhen (%find)
                   (setf (gethash (%keyword it) keywords-ht)
                         (%take-next-args (%num-to-take it))))))
        (dotimes (i (length keywords))
          (unless (%try-parse-kw)
            (return)))
        (append
         (loop
            :for kw :in keywords
            :collect
              (multiple-value-bind (val found?)
                  (gethash (%keyword kw) keywords-ht)
                (if found? val (%default-value kw))))
         (list args)))))

  (defun fmt->format (fmt-string)
    (let ((fmt-string (if (stringp fmt-string)
                          fmt-string
                          (write-to-string fmt-string))))
      (concatenate 'string
                   (loop
                      :for ch :across fmt-string
                      :collect (cond ((char-equal ch #\~) #\:)
                                     ((char-equal ch #\:) #\~)
                                     (t ch))))))  

  (defun transform-fmt-str-form (fmt-str-form
                                 &key delimiter newline? translate?)
    (flet ((%translated-form (translate? form)
             (if translate?
                 (let ((translated-form `(fmt->format ,form)))
                   (if (stringp form)
                       (eval translated-form)
                       translated-form))
                 form)))
      (let* ((format-str-form (%translated-form translate? fmt-str-form))
             (actual-delimiter (when delimiter
                                 (%translated-form translate? delimiter)))
             (format-str-form+d
              (if actual-delimiter
                  `(concatenate 'string ,format-str-form ,actual-delimiter)
                  format-str-form))
             (format-str-form+d
              (if newline?
                  `(concatenate 'string ,format-str-form+d "~%")
                  format-str-form+d)))
        (if (and (stringp format-str-form)
                 (stringp actual-delimiter))
            (eval format-str-form+d)
            format-str-form+d))))

  (defun target-switch (target stream)
    (ecase target
      (:format `(format ,stream))
      (:break '(break))))

  (defun fmt-aux (fmt-str-form fmt-args
                  &key stream newline (translate? t) (target :format))
    `(,@(target-switch target stream)
        ,(transform-fmt-str-form fmt-str-form
                                 :newline? newline
                                 :translate? translate?)
        ,@fmt-args))

  (defun fmt4l-aux (fmt-str-form fmt-args delimiter delimiter+
                    &key stream newline (translate? t) (target :format))
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
      `(,@(target-switch target stream) ,format-str-form ,@fmt-args)))

  (defun fmts-aux (fmt-lists delimiter delimiter+ &key stream (translate? t))
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
              `(get-output-stream-string ,stream-sym))))))
  ) ; eval-when

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
    (fmt-aux (first fmt-string/args) (rest fmt-string/args)
             :stream stream :newline newline :translate? t)))

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
       `:d+' does the same and also ends delimiter to the end.
       Delimiter is also translated (~ -> :)."
  (destructuring-bind (stream newline delimiter delimiter+ fmt-string/args)
      (parse-initial-keywords args '(:s 1 t) :nl '(:d 1) '(:d+ 1))
    (fmt4l-aux (first fmt-string/args) (rest fmt-string/args)
               delimiter delimiter+
               :stream stream :newline newline :translate? t)))

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
    (fmts-aux fmt-lists delimiter delimiter+
              :stream stream :translate? t)))

(defmacro format+ (&rest args)
  "SYNOPSIS
      (format+ [ :s stream ] [ :nl ] format-string format-args)
DESCRIPTION
      You can omit stream argument - t is default.
      `:nl' adds newline in the end (same as :% would)."
  (destructuring-bind (stream newline format-string/args)
      (parse-initial-keywords args '(:s 1 t) :nl)
    (fmt-aux (first format-string/args) (rest format-string/args)
             :stream stream :newline newline :translate? t)))

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
    (fmt4l-aux (first format-string/args) (rest format-string/args)
               delimiter delimiter+
               :stream stream :newline newline :translate? t)))

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
    (fmts-aux format-lists delimiter delimiter+
              :stream stream :translate? t)))

(defmacro echo (&rest args)
  "SYNOPSIS
      (echo [ :-nl ] args)
DESCRIPTION
      Prints args with ~S formatting.
      You can pass `:-nl' as the first argument to avoid newline."
  (if (eq (first args) :-nl)
      `(fmt4l :d " " ":S" ,@(rest args))
      `(fmt4l :nl :d " " ":S" ,@args)))

(defmacro brk (&rest args)
  "SYNOPSIS
      (brk fmt-string fmt-args)
DESCRIPTION
      `fmt-string' is the same as control-string in format,
      but with ~ (tilde) and : (colon) swapped.
      `:nl' adds newline in the end (same as :% would).
      Does the same as break."
  (fmt-aux (first args) (rest args) :target :break :translate? t))

(defmacro brk4l (&rest args)
  "SYNOPSIS
       (brk4l [ :s stream ] [ [ :d | :d+ ] delimiter ]
              fmt-string fmt-args)
DESCRIPTION
       `fmt-string' is the same as control-string in format,
       but with ~ (tilde) and : (colon) swapped.
       Applies `fmt-string' to each arg in fmt-args.
       To put it simply, multiplies `fmt-string' by the number of arguments.
       `:d' allows to set a delimiter to insert between elements.
       `:d+' does the same and also ends delimiter to the end.
       Delimiter is also translated (~ -> :).
       Does the same as break."
  (destructuring-bind (stream delimiter delimiter+ fmt-string/args)
      (parse-initial-keywords args '(:s 1 t) '(:d 1) '(:d+ 1))
    (fmt4l-aux (first fmt-string/args) (rest fmt-string/args)
               delimiter delimiter+ :stream stream
               :target :break :translate? t)))

(defmacro breacho (&rest args)
  "SYNOPSIS
      (brecho args)
DESCRIPTION
      Formats args with ~S formatting."
  `(brk4l :d " " ":S" ,@args))