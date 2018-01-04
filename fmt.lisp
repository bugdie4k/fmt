(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun parse-fmt-args (args)
    (let ((stream t)
          (newline? nil))
      (labels ((%try-parse-kw ()
                 (cond ((eq (first args) :s)
                        (setf stream (second args)
                              args (rest (rest args))))
                       ((eq (first args) :nl)
                        (setf newline? t
                              args (rest args)))
                       ((eq (first args) :nl+)
                        (setf newline? :nl+
                              args (rest args))))))
        (%try-parse-kw)
        (%try-parse-kw)
        (values stream newline? args))))

  (defun fmt->format (fmt-string)
    (concatenate 'string
                 (loop for ch across fmt-string
                    collecting (cond ((char-equal ch #\~) #\:)
                                     ((char-equal ch #\:) #\~)
                                     (t ch)))))

  (defun transform-fmt-str-form (fmt-str-form &key newline? translate?)
    (let* ((format-str-form (if translate?
                                `(fmt->format ,fmt-str-form)
                                fmt-str-form))
           (format-str-form (if newline?
                                `(concatenate 'string ,format-str-form "~%")
                                format-str-form)))
      (if (stringp fmt-str-form)
          (eval format-str-form)
          format-str-form)))

  (defun fmt-aux (stream newline? fmt-str-form fmt-args &key (translate? t))
    `(format ,stream
             ,(transform-fmt-str-form fmt-str-form
                                      :newline? newline?
                                      :translate? translate?)
             ,@fmt-args))

  (defun fmt4l-aux (stream newline? fmt-str-form fmt-args &key (translate? t))
    (let* ((format-str-form-sym (gensym "FORMAT-STR-FORM"))
           (format-str-form
            `(let ((,format-str-form-sym ,(if translate?
                                              `(fmt->format ,fmt-str-form)
                                              fmt-str-form)))
               (format nil "~A~A~A" "~{" ,format-str-form-sym "~}")))
           (format-str-form
            (if newline?
                `(concatenate 'string ,format-str-form "~%")
                format-str-form))
           (format-str-form (if (stringp fmt-str-form)
                                (eval format-str-form)
                                format-str-form)))
      `(format ,stream ,format-str-form (list ,@fmt-args))))

  (defun fmts-aux (stream newline? fmt-lists &key (translate? t))
    (let* ((stream-sym (gensym "STREAM"))
           (stream-src (or stream '(make-string-output-stream))))
      (flet ((%make-format-call (fmt-list newline?)
               `(format ,stream-sym
                        ,@(cons (transform-fmt-str-form (first fmt-list)
                                                        :newline? newline?
                                                        :translate? translate?)
                                (rest fmt-list)))))
        `(let ((,stream-sym ,stream-src))
           ,@(mapcar
              (lambda (fmt-list)
                 (%make-format-call fmt-list newline?))
              (butlast fmt-lists))
           ,(%make-format-call (car (last fmt-lists)) (eq newline? :nl+))
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
  (multiple-value-bind (stream newline? fmt-string/args)
      (parse-fmt-args args)
    (fmt-aux stream newline? (first fmt-string/args) (rest fmt-string/args)
             :translate? t)))

(defmacro fmt4l (&rest args)
  "SYNOPSIS
       (fmt4l [ :s stream ] [ :nl ] fmt-string fmt-args)
DESCRIPTION
       `fmt-string' is the same as control-string in format,
       but with ~ (tilde) and : (colon) swapped.
       Applies `fmt-string' to each arg in fmt-args.
       To put it simply, multiplies `fmt-string' by the number of arguments.
       `:nl' adds newline."
  (multiple-value-bind (stream newline? fmt-string/args)
      (parse-fmt-args args)
    (fmt4l-aux stream newline? (first fmt-string/args) (rest fmt-string/args)
               :translate? t)))

(defmacro fmts (&rest args)
  "SYNOPSIS
      (fmts [ :s stream ] [ :nl | :nl+] (fmt-string fmt-args))
DESCRIPTION
      `fmt-string' is the same as control-string in format,
      but with ~ (tilde) and : (colon) swapped.
      `fmts' is like `fmt' but you can specify several in one form.
      Default `stream' is t.
      `:nl' inserts newlines between fmts.
      `:nl+' inserts newlines between fmts and also after the last one"
  (multiple-value-bind (stream newline? fmt-lists)
      (parse-fmt-args args)
    (fmts-aux stream newline? fmt-lists
              :translate? t)))

(defmacro format2 (&rest args)
  "SYNOPSIS
      (format+ [ :s stream ] [ :nl ] format-string format-args)
DESCRIPTION
      You can omit stream argument - t is default.
      `:nl' adds newline in the end (same as :% would)."
  (multiple-value-bind (stream newline? fmt-string/args)
      (parse-fmt-args args)
    (fmt-aux stream newline? (first fmt-string/args) (rest fmt-string/args)
             :translate? nil)))

(defmacro format4l (&rest args)
  "SYNOPSIS
       (format4l [ :s stream ] [ :nl ] format-string format-args)
DESCRIPTION
       Applies format-string to each arg in format-args.
       To put it simply, multiplies format-string by the number of arguments.
       `:nl' adds newline"
  (multiple-value-bind (stream newline? fmt-string/args)
      (parse-fmt-args args)
    (fmt4l-aux stream newline? (first fmt-string/args) (rest fmt-string/args)
               :translate? nil)))

(defmacro formats (&rest args)
  "SYNOPSIS
      (formats [ :s stream ] [ :nl ] (format-string format-args)+)
DESCRIPTION
      Like `fmt' but you can specify several in one form.
      Default `stream' is t.
      `:nl' inserts newlines between format statements.
      `:nl+' inserts newlines between formats and also after the last one"
  (multiple-value-bind (stream newline? fmt-lists)
      (parse-fmt-args args)
    (fmts-aux stream newline? fmt-lists
              :translate? nil)))

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
