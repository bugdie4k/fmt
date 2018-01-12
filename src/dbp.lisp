(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun lexem->token (lexem lexem-next-args)
    (loop
       :for v :being :the :hash-value :in (kw^get)
       :do (awhen (funcall (first v) (second v) lexem lexem-next-args)
             (return it))))

  (defun symbol->token (args)
    (awhen (symbol-name? (first args))
      (or (lexem->token it (rest args))
          (list (make-instance 'regular-token :form (first args))
                (rest args)))))  

  (defun lexer (args)
    (when args
      (aif (symbol->token args)
           (cons (first it) (lexer (second it)))
           (cons (form->regular-token (first args))
                 (lexer (rest args))))))

  (defun collect-options-returns-eval-list (tokens options)
    (flet ((%eval-list (token)
             (awhen (generated-symbol token)
               (list it (form token)))))
      (loop
         :with cleaned-token-list = nil
         :with returns = nil
         :with eval-list = nil
         :for token :in tokens
         :do
           (cond ((typep token 'option-token)
                  (setf (gethash (name token) options)
                        (value token)))
                 ((typep token 'return-token)
                  (awhen (%eval-list token) (push it eval-list))
                  (push token returns)
                  (when (print? token)
                    (push token cleaned-token-list)))
                 ((typep token 'regular-token)
                  (awhen (%eval-list token) (push it eval-list))
                  (push token cleaned-token-list))
                 (t (push token cleaned-token-list)))
         :finally
           (return (values options returns (reverse eval-list)
                           (reverse cleaned-token-list))))))

  (defun take-section (tokens &optional taken-tokens)
    (if (or (not tokens)
            (typep (first tokens) 'section-token))
        (values (reverse taken-tokens) tokens)
        (take-section (rest tokens)
                      (cons (first tokens) taken-tokens))))

  (defun divide-by-sections (tokens)
    (let (prefix message)
      (labels
          ((%parse (tokens)
             (if (not tokens)
                 (values prefix message)
                 (let* ((tok (first tokens))
                        (tokens-to-take
                         (if (typep tok 'section-token) (rest tokens) tokens)))
                   (multiple-value-bind (taken-tokens rest-tokens)
                       (take-section tokens-to-take)
                     (cond ((typep tok 'prefix-section)
                            (setf prefix (append prefix taken-tokens)))
                           ((typep tok 'message-section)
                            (setf message (append message taken-tokens)))
                           ;; message is the default
                           (t (setf message (append message taken-tokens))))
                     (%parse rest-tokens))))))
        (%parse tokens))))

  (defun parser (tokens)
    (mv-let* (((options returns eval-list tokens)
               (collect-options-returns-eval-list tokens
                                                  (kw^default-options-ht)))
              ((prefix-tokens message-tokens)
               (divide-by-sections tokens)))      
      (values prefix-tokens message-tokens eval-list returns options)))

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
                 (%+word-delimiter (write-to-string form)))
                (t (values (%+word-delimiter format-letter)
                           (or generated-symbol
                               form))))))))

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
                   (multiple-value-bind (f-str f-arg)
                       (translate-token
                        (first token-list) options
                        :word-delimiter? (%word-delimiter? (rest token-list)))
                     (%translate (rest token-list)
                                 (append format-control-chars
                                         (coerce f-str 'list))
                                 (cons-if-truthy f-arg format-args))))))
      (when token-list
        (multiple-value-bind (format-control-string format-args)
            (%translate token-list)
          `(format nil ,format-control-string ,@format-args)))))

  (defmacro dbp (&body body)
    "dbp is short for debug print.
It is a macro with a small markup syntax for a better experience
of debugging with debug prints."
    (mv-let* (((prefix message eval-list returns options)
               (parser (lexer body)))
              ((prefix-format)
               (translate-tokens prefix options))
              ((message-format)
               (translate-tokens message options)))
      `(,@(if eval-list
              `(let ,eval-list)
              '(progn))
         (dbp-print-message :prefix ,prefix-format
                            :message ,message-format
                            :options ,options)
          (values
           ,@(mapcar
              (lambda (return-token)
                (aif (generated-symbol return-token) it (form return-token)))
              returns)))))

  ) ; eval-when
