(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun kw-keyword (keyword-def)
    (if (listp keyword-def) (first keyword-def) keyword-def))

  (defun kw-args-num-to-take (keyword-def)
    (if (listp keyword-def) (second keyword-def) 0))

  (defun kw-default-value (keyword-def)
    (if (listp keyword-def) (third keyword-def) nil))

  (defun find-kw (arg parsed-keywords keyword-definitions)
    (unless (assoc arg parsed-keywords)
      (find-if (curry #'eq arg)
               keyword-definitions :key #'kw-keyword)))

  (defun take-next-args (next-args num)
    (cond ((= num 0) (values t next-args))
          ((= num 1) (values (first next-args) (rest next-args)))
          ((>= num 2) (values (subseq next-args 0 num)
                              (nthcdr num next-args)))
          (t (error "cannot take ~A (< 0) args" num))))

  (defun parse-initial-keywords (args &key keyword-definitions)
    (let ((kw-len (length keyword-definitions)))
      (labels ((%parse (args &optional parsed-keywords (i 0))
                 (if (>= i kw-len)
                     (values parsed-keywords args)
                     (aif (find-kw (first args) parsed-keywords
                                   keyword-definitions)
                          (multiple-value-bind (kw-value rest-args)
                              (take-next-args (rest args)
                                              (kw-args-num-to-take it))
                            (%parse rest-args
                                    (acons (kw-keyword it) kw-value
                                           parsed-keywords)
                                    (1+ i)))
                          (values parsed-keywords args))))
               (%collect-kw-return-list (parsed-keywords)
                 (loop
                    :for kw-def :in keyword-definitions
                    :collect
                      (aif (assoc (kw-keyword kw-def) parsed-keywords)
                           (cdr it)
                           (kw-default-value kw-def)))))
        (multiple-value-bind (parsed-keywords rest-args)
            (%parse args)
          (append
           (%collect-kw-return-list parsed-keywords)
           (list rest-args))))))

  (defun translate-pair (ch1 ch2 fmt-chars)
    (cond ((char= ch1 #\~)
           (values (rest fmt-chars) (list #\~ #\~)))
          ((and (char= ch1 #\:)
                (or (not ch2)
                    (not (char= ch2 #\:))))
           (values (rest fmt-chars) (list #\~)))
          ((and (char= ch1 #\:)
                (char= ch2 #\:))
           (values (rest (rest fmt-chars)) (list #\:)))
          (t (values (rest fmt-chars) (list ch1)))))

  (defun translate-fmt-to-format (fmt-chars &optional translated-chars)
    (if (not fmt-chars)
        translated-chars
        (multiple-value-bind (rest-fmt-chars next-char-list)
            (translate-pair (first fmt-chars)
                            (second fmt-chars)
                            fmt-chars)
          (translate-fmt-to-format
           rest-fmt-chars
           (append translated-chars
                   next-char-list)))))

  (defun fmt->format (fmt-string)
    (let ((fmt-string (if (stringp fmt-string)
                          fmt-string
                          (write-to-string fmt-string))))
      (concatenate
       'string
       (translate-fmt-to-format
        (coerce fmt-string 'list)))))

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
                 (or (not actual-delimiter)
                     (stringp actual-delimiter)))
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
      (labels ((%make-ordinary-format-call (fmt-list)
                 `(format ,stream-sym
                          ,@(cons (transform-fmt-str-form
                                   (first fmt-list)
                                   :translate? translate?)
                                  (rest fmt-list))))
               (%make-delim-format-call ()
                 (when delimiter
                   `(format ,stream-sym
                            ,(transform-fmt-str-form delimiter
                                                     :translate? translate?))))
               (%make-format-call (fmt-list)
                 (aif (symbol-name? (first fmt-list))
                      (cond ((string-equal it "l")
                             `(fmt4l :s ,stream-sym ,@(rest fmt-list)))
                            ((string-equal it "s")
                             `(fmts :s ,stream-sym ,@(rest fmt-list)))
                            (t (%make-ordinary-format-call fmt-list)))
                      (%make-ordinary-format-call fmt-list))))
        `(let ((,stream-sym ,stream-src))
           ,@(mappend
              (lambda (fmt-list)
                (let* ((delim (%make-delim-format-call))
                       (delim (when delim (list delim))))
                  (append
                   (list (%make-format-call fmt-list))
                   delim)))
              (butlast fmt-lists))
           ,(%make-format-call (first (last fmt-lists)))
           ,@(when delimiter+
               (list (%make-delim-format-call)))
           ,@(unless stream
               (list `(get-output-stream-string ,stream-sym)))))))

  ) ; eval-when

(defmacro fmt (&rest args)
  "SYNOPSIS
      (fmt [ :s stream ] [ :nl ] fmt-string fmt-arg*)
DESCRIPTION
      Same as format but with different syntax.

      `fmt-string' is the same as control-string in format,
      but with colon (:) as the character
      to introduce the directive instead of tilde (~).

      You can omit stream argument - t is default.

      `:nl' adds newline in the end (same as :% would)."
  (destructuring-bind (stream newline fmt-string/args)
      (parse-initial-keywords args :keyword-definitions '((:s 1 t) :nl))
    (fmt-aux (first fmt-string/args) (rest fmt-string/args)
             :stream stream :newline newline :translate? t)))

(defmacro fmt4l (&rest args)
  "SYNOPSIS
       (fmt4l [ :s stream ] [ :nl ] [ [ :d | :d+ ] delimiter ]
              fmt-string fmt-arg*)
DESCRIPTION
      Allows to apply one `fmt-string' to each argument of `fmt-arg*'.

      `fmt-string' is the same as control-string in format,
      but with colon (:) as the character
      to introduce the directive instead of tilde (~).

      You can omit stream argument - t is default.

      `:nl' adds newline to the end.

      `:d' allows to set a delimiter to insert between elements.

      `:d+' does the same as `:d' and also adds delimiter to the end.

       Delimiter is also translated (same as `fmt-string')
       so you can specify ':%' as a delimiter to delimit with newline."
  (destructuring-bind (stream newline delimiter delimiter+ fmt-string/args)
      (parse-initial-keywords
       args
       :keyword-definitions '((:s 1 t) :nl (:d 1) (:d+ 1)))
    (fmt4l-aux (first fmt-string/args) (rest fmt-string/args)
               delimiter delimiter+
               :stream stream :newline newline :translate? t)))

(defmacro fmts (&rest args)
  "SYNOPSIS
      (fmts [ :s stream ] [ [ :d | :d+] delimiter ]
            (fmt-string fmt-arg*)+)
DESCRIPTION
      Allows to have multiple fmt calls in one form.

      `fmt-string' is the same as control-string in format,
      but with colon (:) as the character
      to introduce the directive instead of tilde (~).

      You can omit stream argument - t is default.

      `:d' allows to set a delimiter to insert between fmts.

      `:d+' does the same and also ends delimiter to the end.

      Delimiter is also translated (same as `fmt-string')
      so you can specify ':%' as a delimiter to delimit with newline."
  (destructuring-bind (stream delimiter delimiter+ fmt-lists)
      (parse-initial-keywords
       args
       :keyword-definitions '((:s 1 t) (:d 1) (:d+ 1)))
    (fmts-aux fmt-lists delimiter delimiter+
              :stream stream :translate? t)))

(defmacro format+ (&rest args)
  "SYNOPSIS
      (format+ [ :s stream ] [ :nl ] control-string format-arg*)
DESCRIPTION
      You can omit stream argument - t is default.
      `:nl' adds newline in the end (same as ~% would)."
  (destructuring-bind (stream newline format-string/args)
      (parse-initial-keywords
       args
       :keyword-definitions '((:s 1 t) :nl))
    (fmt-aux (first format-string/args) (rest format-string/args)
             :stream stream :newline newline :translate? t)))

(defmacro format4l (&rest args)
  "SYNOPSIS
       (format4l [ :s stream ] [ :nl ] [ [ :d | :d+ ] delimiter ]
                 control-string format-arg*)
DESCRIPTION
      Allows to apply one `control-string' to each argument of `format-arg*'.

      You can omit stream argument - t is default.

      `:nl' adds newline to the end.

      `:d' allows to set a delimiter to insert between elements.

      `:d+' does the same as `:d' and also adds delimiter to the end.

       Delimiter can also have format directives inside
       that do not require arguments such as ~%."
  (destructuring-bind (stream newline delimiter delimiter+ format-string/args)
      (parse-initial-keywords
       args
       :keyword-definitions '((:s 1 t) :nl (:d 1) (:d+ 1)))
    (fmt4l-aux (first format-string/args) (rest format-string/args)
               delimiter delimiter+
               :stream stream :newline newline :translate? t)))

(defmacro formats (&rest args)
  "SYNOPSIS
      (formats [ :s stream ] [ [ :d | :d+] delimiter ]
               (format-string format-arg*)+)
DESCRIPTION
      Allows to have multiple format calls in one form.

      You can omit stream argument - t is default.

      `:d' allows to set a delimiter to insert between fmts.

      `:d+' does the same and also ends delimiter to the end."
  (destructuring-bind (stream delimiter delimiter+ format-lists)
      (parse-initial-keywords
       args
       :keyword-definitions '((:s 1 t) (:d 1) (:d+ 1)))
    (fmts-aux format-lists delimiter delimiter+
              :stream stream :translate? t)))

(defmacro echo (&rest args)
  "SYNOPSIS
      (echo [ :-nl ] arg*)
DESCRIPTION
      Prints args with ~S formatting.
      You can pass `:-nl' as the first argument to avoid newline."
  (if args
      (if (eq (first args) :-nl)
          `(fmt4l :d+ " " ":S" ,@(rest args))
          `(fmt4l :nl :d " " ":S" ,@args))
      `(format t "~%")))

(defmacro brk (&rest args)
  "SYNOPSIS
      (brk fmt-string fmt-args)
DESCRIPTION
      Same as break, but with different syntax.

      `fmt-string' is the same as control-string in format,
      but with colon (:) as the character
      to introduce the directive instead of tilde (~).

      `:nl' adds newline in the end (same as :% would)."
  (fmt-aux (first args) (rest args) :target :break :translate? t))

(defmacro brk4l (&rest args)
  "SYNOPSIS
       (brk4l [ :s stream ] [ [ :d | :d+ ] delimiter ]
              fmt-string fmt-args)
DESCRIPTION
      Allows to apply one `fmt-string' to each argument of `fmt-args'.

      `fmt-string' is the same as control-string in format,
      but with colon (:) as the character
      to introduce the directive instead of tilde (~).

      `:d' allows to set a delimiter to insert between elements.

      `:d+' does the same and also ends delimiter to the end.

      Delimiter is also translated (same as `fmt-string')
      so you can specify ':%' as a delimiter to delimit with newline."
  (destructuring-bind (stream delimiter delimiter+ fmt-string/args)
      (parse-initial-keywords
       args
       :keyword-definitions '((:s 1 t) (:d 1) (:d+ 1)))
    (fmt4l-aux (first fmt-string/args) (rest fmt-string/args)
               delimiter delimiter+ :stream stream
               :target :break :translate? t)))

(defmacro breacho (&rest args)
  "SYNOPSIS
      (brecho args)
DESCRIPTION
      Same as break but all args are formatted with ~S directive."
  `(brk4l :d " " ":S" ,@args))
