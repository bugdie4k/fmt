(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass+ token ())
  (defclass+ regular-token (token) form)
  (defclass+ newline-token (token) conditional?)
  (defclass+ delim-token (token) pattern)
  (defclass+ return-token (token) form print?)
  (defclass+ option-token (token) name value)

  (defmethod print-object ((tok token) stream)
    (format stream ".~A." (token-designator tok)))

  (defmethod token-designator ((tok regular-token))
    (format nil "R<~S>" (form tok)))

  (defmethod token-designator ((tok newline-token))
    (if (conditional? tok) "CNL" "NL"))

  (defmethod token-designator ((tok delim-token))
    (format nil "D<~A>" (pattern tok)))

  (defmethod token-designator ((tok return-token))
    (format nil "~A<~A>" (if (print? tok) "PRET" "RET") (form tok)))

  (defmethod token-designator ((tok option-token))
    (format nil "O<~A:~A>" (name tok) (value tok)))

  (defun parse-take1 (next-args)
    (values (make-instance 'regular-token
                           :form (first next-args))
            (rest next-args)))

  (defun parse-delimiter (next-args)
    (values (make-instance 'delim-token
                           :pattern (write-to-string
                                     (first next-args)))
            (rest next-args)))

  (defun parse-newline (conditional? next-args)
    (values (make-instance 'newline-token :conditional? conditional?)
            next-args))

  (defun parse-return (print? next-args)
    (values (make-instance 'return-token :form (first next-args)
                           :print? print?)
            (rest next-args)))

  (defclass+ keywords ()
    (section-designators (make-hash-table))
    (markup (make-hash-table))
    (options (make-hash-table)))

  (defvar *keywords* (make-instance 'keywords))

  (defmacro ^defkeywords (&rest definitions-by-type)
    (flet ((%accessor (slot-kw)
             (ecase slot-kw
               ((:section-designators :markup :options)
                (intern (symbol-name slot-kw)))))
           (%add-keyword (accessor name value)
             `(setf
               (gethash ,name (,accessor *keywords*))
               ,value)))
      `(progn ,@(mappend
                 (lambda (definition)
                   (destructuring-bind (keyword-type &rest entries) definition
                     (let ((accessor (%accessor keyword-type)))
                       (mapcar
                        (lambda (entry)
                          (destructuring-bind (name &rest value) entry
                            (let ((value (if (cdr value)
                                             `(list ,@value)
                                             (car value))))
                              (%add-keyword accessor name value))))
                        entries))))
                 definitions-by-type))))

  (defun parse-word-delimiter (next-args)
    (values (format nil "~A" (first next-args))
            (rest next-args)))

  (defun make-parse-set-to (value)
    (lambda (next-args)
      (values value next-args)))

  (^defkeywords
   (:section-designators
    (:prefix  "p>")
    (:message "m>"))
   (:markup
    (:literally           "l"   #'parse-take1)
    (:delimiter           "d"   #'parse-delimiter)
    (:newline             "nl"  (curry #'parse-newline nil))
    (:conditional-newline "cnl" (curry #'parse-newline t))
    (:return              "r"   (curry #'parse-return nil))
    (:print-return        "pr"  (curry #'parse-return t)))
   (:options
    (:words-delimiter  "?wd"             " " #'parse-word-delimiter)
    (:stream           "?s"              t   #'parse-take1)
    (:delimiter-length "?dl"             60  #'parse-take1)
    (:format-letter    "?format"         "S" #'parse-take1)
    (:reset-counter    "?rsc"            nil (make-parse-set-to t))
    (:no-end-newline   "?no-end-newline" nil (make-parse-set-to t))
    (:no-counter       "?no-counter"     nil (make-parse-set-to t))
    (:no-clip          "?no-clip"        nil (make-parse-set-to t))))

  (defun ^default-options-ht ()
    (let ((ht (make-hash-table)))
      (maphash
       (lambda (name settings)
         (let ((default (second settings)))
           (setf (gethash name ht) default)))
       (options *keywords*))
      ht))

  (defun ^section-designators-alist ()
    (hash-table-alist (section-designators *keywords*)))

  (defun ^markup-alist ()
    (hash-table-alist (markup *keywords*)))

  (defun ^options-alist ()
    (hash-table-alist (options *keywords*)))

  (defun ^section-designators-names ()
    (hash-table-values
     (section-designators *keywords*)))

  (defun symbol-name? (arg)
    (and (symbolp arg) (symbol-name arg)))

  (defun section-designator? (arg)
    (let ((arg-name (symbol-name? arg)))
      (find-if (curry #'string-equal arg-name)
               (^section-designators-names))))

  (defun parse-markup-lexem (lexem next-args)
    (loop
       :for (name . (value parse-fn))
       :in (^markup-alist)
       :when (string-equal value lexem)
       :do (return (funcall parse-fn next-args))))

  (defun parse-option-lexem (lexem next-args)
    (loop
       :for (name . (value default parse-fn))
       :in (^options-alist)
       :when (string-equal value lexem)
       :do (multiple-value-bind (value-to-use rest-args)
               (funcall parse-fn next-args)
             (return
               (values (make-instance 'option-token
                                      :name name
                                      :value value-to-use)
                       rest-args)))))

  (defun parse-lexem (lexem args)
    (let* ((next-args (rest args)))
      (if (and (string-equal (subseq lexem 0 1) "d")
               (not (string-equal (subseq lexem 1) "")))
          ;; NOTE: special syntax for d
          (values (make-instance 'delim-token
                                 :pattern (subseq lexem 1))
                  next-args)
          (multiple-value-bind (markup-token rest-args)
              (parse-markup-lexem lexem next-args)
            (if markup-token
                (values markup-token rest-args)
                (multiple-value-bind (option rest-args)
                    (parse-option-lexem lexem next-args)
                  (if option
                      (values option rest-args)
                      (values (make-instance 'regular-token
                                             :form (first args))
                              next-args))))))))

  (defun parse-markup (args &optional tokens)
    (if (not args)
        (values (reverse tokens) nil)
        (aif (symbol-name? (first args))
             (if (section-designator? (first args))
                 (values (reverse tokens) args)
                 (multiple-value-bind (token rest-args)
                     (parse-lexem it args)
                   (parse-markup rest-args (cons token tokens))))
             (parse-markup (rest args)
                           (cons (make-instance 'regular-token
                                                :form (first args))
                                 tokens)))))

  (defun section-slot-name (lexem)
    (dolist (entry (^section-designators-alist))
      (destructuring-bind (name . value) entry
        (when (string-equal value lexem)
          (return (intern (symbol-name name) :fmt))))))

  (defclass+ parsed ()
    prefix
    message
    (options (^default-options-ht)))

  (defun parse-sections (args &optional (parsed (make-instance 'parsed)))
    (if (not args)
        (values parsed nil)
        (multiple-value-bind (slot-name-sym args-to-parse)
            (aif (aand (symbol-name? (first args))
                       (section-slot-name it))
                 (values it (rest args))
                 ;; NOTE: if no section is specified as the first arg
                 ;;       -> it's message ('m>') section
                 (values 'message args))
          (multiple-value-bind (token-list rest-args)
              (parse-markup args-to-parse)
            (setf (slot-value parsed slot-name-sym) token-list)
            (parse-sections rest-args parsed)))))

  (defun pick-options (token-list options)
    (loop
       :with cleaned-token-list = nil
       :for token in token-list
       :do (if (typep token 'option-token)
               (setf (gethash (name token) options)
                     (value token))
               (push token cleaned-token-list))
       :finally (return (values options (reverse cleaned-token-list)))))

  (defun parse-body (body)
    (with-slots (prefix message options)
        (parse-sections body)
      (multiple-value-bind (redefined-options cleaned-prefix)
          (pick-options prefix options)
        (multiple-value-bind (redefined-options cleaned-message)
            (pick-options message redefined-options)
          (make-instance 'parsed
                         :prefix  cleaned-prefix
                         :message cleaned-message
                         :options redefined-options)))))

  (defgeneric translate-token (tok options)
    (:documentation
     "Translate token returning up to four values:
control-string, format-argument, return-form, return-form-print?"))

  (defmethod translate-token ((tok regular-token) options)
    (let ((format-letter
           (string+ "~" (gethash :format-letter options)))
          (word-delimiter
           (gethash :words-delimiter options)))
      (labels ((%stringable? (form)
                 (or (numberp form)
                     (characterp form)
                     (keywordp form)))
               (%translate-el/string (str)
                 (string+ str word-delimiter)))
        (with-slots (form) tok
          (cond ((stringp form) (%translate-el/string form))
                ((%stringable? form)
                 (%translate-el/string (write-to-string form)))
                (t (values (string+ format-letter word-delimiter)
                           form)))))))

  (defmethod translate-token ((tok delim-token) options)
    (let* ((delim-len (gethash :delimiter-length options))
           (pattern (pattern tok))
           (pattern-len (length pattern)))
      (flet ((%make-delim ()
               (with-output-to-string (s)
                 (loop
                    :with cur-len = 0
                    :while (< cur-len delim-len)
                    :do
                      (format s "~A" pattern)
                      (setf cur-len (+ cur-len pattern-len))))))
        (string+ "~&" (subseq (%make-delim) 0 delim-len) "~&"))))

  (defmethod translate-token ((tok newline-token) options)
    (if (conditional? tok) "~&" "~%"))

  (defmethod translate-token ((tok return-token) options)
    (let ((sym (gensym "RETURNED"))
          (format-letter
           (string+ "~" (gethash :format-letter options)))
          (word-delimiter
           (gethash :words-delimiter options)))
      (if (print? tok)
          (values (string+ format-letter word-delimiter) sym
                  (list sym (form tok)))
          (values "" nil
                  (list sym (form tok))))))

  (defun translate-tokens (token-list options)
    "Translates markup token list to format form"
    (labels ((%translate (token-list
                          &optional format-control-chars format-args returns)
               (if (not token-list)
                   (values (coerce format-control-chars 'string)
                           (reverse format-args)
                           returns)
                   (multiple-value-bind (f-str f-arg return)
                       (translate-token (first token-list) options)
                     (let* ((format-control-chars (append format-control-chars
                                                  (coerce f-str 'list)))
                            (format-args (if f-arg
                                             (cons f-arg format-args)
                                             format-args))
                            (returns (if return
                                         (cons return returns)
                                         returns)))
                       (%translate (rest token-list)
                                   format-control-chars
                                   format-args
                                   returns))))))
      (multiple-value-bind (format-control-string format-args returns)
          (%translate token-list)
        (values `(format nil ,format-control-string ,@format-args)
                returns))))

  ) ; eval-when

;; TODO (defvar *dbp-options* nil)
(defvar *dbp-counter* 0)

(defun dbp-reset-counter ()
  (setf *dbp-counter* 0))

(defmacro dbp (&body body)
  (with-slots (prefix message options)
      (parse-body body)
    (multiple-value-bind (prefix-format prefix-returns)
        (translate-tokens prefix options)
      (multiple-value-bind (message-format message-returns)
          ))
    (let ((prefix-format )
          (message-format (translate-tokens message options)))
      (echo prefix-format)
      (echo message-format))))

(defun test ()
  (with-slots (prefix message options)
      (parse-body
       '(1 2 3 d-- (+ 1 2) (* 4 2) nl :a cnl :b nl nl :c nl cnl cnl :l :d r 1 pr 2))
    (echo prefix)
    (echo message)
    (echo (alexandria::hash-table-plist options))
    (multiple-value-bind (fs fa rs)
        (translate-tokens message options)
      (echo fs)
      (echo fa)
      (echo rs)
      (echo :>>>>>>>>>)
      (eval `(format t ,fs ,@fa)))))
