(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)
  
  (defgeneric translate-token (token options &key word-delimiter?)
    (:documentation
     "Translate token returning two values:
format-control-string, format-argument"))

  (defmethod translate-token ((token regular-token) options
                              &key word-delimiter?)
    (let ((format-letter
           (string+ "~" (gethash :format-letter options)))
          (word-delimiter
           (gethash :word-delimiter options)))
      (flet ((%+word-delimiter (str)
               (if word-delimiter?
                   (string+ str word-delimiter)
                   str))
             (%stringable? (form)
               (or (numberp form)
                   (characterp form)
                   (keywordp form))))
        (with-slots (form generated-symbol) token
          (cond ((stringp form) (%+word-delimiter form))
                ((%stringable? form)
                 (%+word-delimiter (format nil format-letter form)))
                (t (values (%+word-delimiter format-letter)
                           (or generated-symbol
                               form)
                           t)))))))

  (defmethod translate-token ((token return-token) options
                              &key word-delimiter?)
    (translate-token
     (make-instance 'regular-token
                    :form (form token)
                    :generated-symbol (generated-symbol token))
     options
     :word-delimiter? word-delimiter?))

  (defun double~tildes (pattern)
    (with-output-to-string (s)
      (loop
         :for ch :across pattern
         :do
           (format s "~C" ch)
           (when (char= ch #\~)
             (format s "~C" #\~)))))

  (defmethod translate-token ((token delim-token) options
                              &key word-delimiter?)
    (let* ((delim-len (gethash :delimiter-width options))
           (pattern (pattern token))
           (pattern-len (length pattern)))
      (flet ((%make-delim ()
               (with-output-to-string (s)
                 (loop
                    :with cur-len = 0
                    :while (< cur-len delim-len)
                    :do
                      (format s "~A" pattern)
                      (setf cur-len (+ cur-len pattern-len))))))
        (string+ "~&" (double~tildes (subseq (%make-delim) 0 delim-len))
                 (if word-delimiter? "~%" "")))))

  (defmethod translate-token ((token newline-token) options
                              &key word-delimiter?)
    (declare (ignore word-delimiter?))
    (if (conditional? token) "~&" "~%"))

  (defun translate-tokens (token-list options)
    "Translates markup token list to format form"
    (labels ((%word-delimiter? (rest-token-list)
               (and rest-token-list
                    (not (typep (first rest-token-list) 'delim-token))
                    (not (typep (first rest-token-list) 'newline-token))))
             (%translate (token-list
                          &optional format-control-chars format-args)
               (if (not token-list)
                   (values (coerce format-control-chars 'string)
                           (reverse format-args))
                   (multiple-value-bind (f-str f-arg f-arg-set)
                       (translate-token
                        (first token-list) options
                        :word-delimiter? (%word-delimiter? (rest token-list)))
                     (%translate (rest token-list)
                                 (append format-control-chars
                                         (coerce f-str 'list))
                                 (if f-arg-set
                                     (cons f-arg format-args)
                                     format-args))))))
      (when token-list
        (multiple-value-bind (format-control-string format-args)
            (%translate token-list)
          `(format nil ,format-control-string ,@format-args)))))

  ) ; eval-when
