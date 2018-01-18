(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defun lexem->token (lexem lexem-next-args)
    (loop
       :for v :being :the :hash-value :in (kw^get)
       :do (awhen (funcall (first v) (second v) lexem lexem-next-args)
             (return it))))

  ;; NOTE: special syntax for d here
  (defun special-syntax-symbol? (sym-name args)
    (when (and (keywordp (first args))
               (> (length sym-name) 1)
               (string-equal (subseq sym-name 0 1) "d"))
      (list (make-instance 'delim-token
                           :pattern (subseq sym-name 1))
            (rest args))))

  (defun symbol->token (args)
    (awhen (symbol-name? (first args))
      (or (special-syntax-symbol? it args)
          (lexem->token it (rest args))
          (list (form->regular-token (first args))
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

  ) ; eval-when
