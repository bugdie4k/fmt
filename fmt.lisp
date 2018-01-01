(in-package :fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-fmt-args (args)
    (let* ((default-stream? (not (eq (first args) :s)))
           (stream (if default-stream? t (second args)))
           (args-but-stream (if default-stream? args (rest (rest args)))))
      (values stream (first args-but-stream) (rest args-but-stream))))

  (defun %parse-fmts-args (args)
    (let ((stream t)
          (newline? nil))
      (labels ((%try-parse-kw ()
                 (cond ((eq (first args) :s)
                        (setf stream (second args)
                              args (rest (rest args))))
                       ((eq (first args) :nl)
                        (setf newline? t
                              args (rest args))))))
        (%try-parse-kw)
        (%try-parse-kw)
        (values stream newline? args))))

  (defun %translate-fmt-string (fmt-string)
    (concatenate 'string
                 (loop for ch across fmt-string
                       collecting (cond ((char-equal ch #\~) #\:)
                                        ((char-equal ch #\:) #\~)
                                        (t ch)))))

  (defun %4l-aux (stream format-string-for-arg format-args translate-fn)
    (let ((args-num (length format-args))
          (format-string-for-arg (funcall translate-fn format-string-for-arg)))
      `(format ,stream ,(with-output-to-string (s) (loop repeat args-num collect (princ format-string-for-arg s)))
                ,@format-args)))

  (defun %s-aux (stream newline? format-lists translate-fn)
    `(progn
       ,@(mapcar
          (lambda (format-list)
            (let ((new-format (funcall translate-fn (car format-list)))
                  (add-to-format (if newline? "~%" "")))
              `(format ,stream
                       ,@(cons (concatenate 'string new-format add-to-format)
                               (rest format-list)))))
          format-lists))))

(defmacro fmt (&rest args)
  "SYNOPSIS
      (fmt [ :s stream ] [ :nl ] fmt-string fmt-args)
DESCRIPTION
      Does the same as format, but ~ (tilde) and : (colon)
      are swapped in format-string (fmt-string). 
      You can omit stream argument - t is default."
  (multiple-value-bind (stream fmt-string fmt-args)
      (%parse-fmt-args args)
    `(format ,stream ,(%translate-fmt-string fmt-string) ,@fmt-args)))


(defmacro fmt4l (&rest args)
  "SYNOPSIS
       (fmt4l [ :s stream ] [ :nl ] fmt-string fmt-args)
DESCRIPTION
       Applies fmt-string to each arg in fmt-args.
       To put it simply, multiplies fmt-string by the number of arguments"
  (multiple-value-bind (stream fmt-string-for-arg fmt-args)
      (%parse-fmt-args args)
    (%4l-aux stream fmt-string-for-arg fmt-args #'%translate-fmt-string)))

(defmacro fmts (&rest args)
  "SYNOPSIS
      (fmts [ :s stream ] [ :nl ] (fmt-string fmt-args))
DESCRIPTION
      Like `fmt' but you can specify several in one form.
      Default `stream' is t.
      `:nl' as the first or second arg inserts newlines between fmts."
  (multiple-value-bind (stream newline? fmt-lists) (%parse-fmts-args args)
    (%s-aux stream newline? fmt-lists #'%translate-fmt-string)))
