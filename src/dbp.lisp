(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass+ token ())
  (defclass+ regular-token (token) form)
  (defclass+ newline-token (token) conditional?)
  (defclass+ delim-token (token) pattern)
  (defclass+ return-token (token) form generated-symbol print?)
  (defclass+ option-token (token) name value)

  (defmethod print-object ((tok token) stream)
    (format stream ".~A." (token-designator tok)))

  (defgeneric token-designator (tok)
    (:documentation "Return string with textual token representation"))

  (defmethod token-designator ((tok regular-token))
    (format nil "R<~S>" (form tok)))

  (defmethod token-designator ((tok newline-token))
    (if (conditional? tok) "CNL" "NL"))

  (defmethod token-designator ((tok delim-token))
    (format nil "D<~A>" (pattern tok)))

  (defmethod token-designator ((tok return-token))
    (format nil "~A<~S = ~A>"
            (if (print? tok) "PRET" "RET")
            (generated-symbol tok)
            (form tok)))

  (defmethod token-designator ((tok option-token))
    (format nil "O<~A:~A>" (name tok) (value tok)))

  (defun parse-kw/literally (lexem-next-args)
    (values (make-instance 'regular-token
                           :form (first lexem-next-args))
            (rest lexem-next-args)))

  (defun parse-kw/delimiter (lexem-next-args)
    (values (make-instance 'delim-token
                           :pattern (write-to-string
                                     (first lexem-next-args)))
            (rest lexem-next-args)))

  (defun parse-kw/newline (conditional? lexem-next-args)
    (values (make-instance 'newline-token :conditional? conditional?)
            lexem-next-args))

  (defun parse-kw/return (print? lexem-next-args)
    (values (make-instance 'return-token
                           :form (first lexem-next-args)
                           :generated-symbol (gensym "RETURNED")
                           :print? print?)
            (rest lexem-next-args)))

  (defclass+ keywords ()
    (section-designators (make-hash-table))
    (markup (make-hash-table))
    (options (make-hash-table))
    (returns (make-hash-table)))

  ;; functions that access *keywords* are prefixed with kw^
  (defvar *keywords* (make-instance 'keywords))

  (defun kw^keywords-slots-as-keywords ()
    (loop
       :for slot :in (sb-mop::class-slots (find-class 'keywords))
       :collect (intern (symbol-name (sb-mop::slot-definition-name slot))
                        :keyword)))

  (defmacro kw^defkeywords (&rest definitions-by-type)
    (flet ((%accessor (slot-kw)
             (if (find slot-kw (kw^keywords-slots-as-keywords))
                 (intern (symbol-name slot-kw))
                 (error "Bad kw^defkeywords form:~%~
                         The keywords class (~A) has no slot with the name ~A"
                        (find-class 'keywords) (symbol-name slot-kw))))
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

  (defun parse-kw-opt/take1-to-string (lexem-next-args)
    (values (format nil "~A" (first lexem-next-args))
            (rest lexem-next-args)))

  (defun parse-kw-opt/take1 (lexem-next-args)
    (values (first lexem-next-args)
            (rest lexem-next-args)))

  (defun parse-kw-opt/make-set-to (value)
    (lambda (lexem-next-args)
      (values value lexem-next-args)))

  (kw^defkeywords
   (:section-designators
    (:prefix  "p>")
    (:message "m>"))
   (:markup
    (:literally           "l"   #'parse-kw/literally)
    (:delimiter           "d"   #'parse-kw/delimiter)
    (:newline             "nl"  (curry #'parse-kw/newline nil))
    (:conditional-newline "cnl" (curry #'parse-kw/newline t)))
   (:returns
    (:return       "r="  (curry #'parse-kw/return nil))
    (:print-return "pr=" (curry #'parse-kw/return t)))
   (:options
    (:word-delimiter  "?wd"             " " #'parse-kw-opt/take1-to-string)
    (:format-letter   "?fletter"        "S" #'parse-kw-opt/take1-to-string)
    (:stream          "?s"              t   #'parse-kw-opt/take1)
    (:delimiter-width "?dw"             60  #'parse-kw-opt/take1)
    (:counter-width   "?counter-w"      3   #'parse-kw-opt/take1)
    (:prefix-width    "?prefix-w"       8   #'parse-kw-opt/take1)
    (:break           "?break"          nil (parse-kw-opt/make-set-to t))
    (:cut-counter     "?cut-counter"    nil (parse-kw-opt/make-set-to t))
    (:cut-prefix      "?cut-prefix"     nil (parse-kw-opt/make-set-to t))
    (:reset-counter   "?rsc"            nil (parse-kw-opt/make-set-to t))
    (:no-end-newline  "?no-end-newline" nil (parse-kw-opt/make-set-to t))
    (:no-counter      "?no-counter"     nil (parse-kw-opt/make-set-to t))
    (:no-clip         "?no-clip"        nil (parse-kw-opt/make-set-to t))))

  (defun kw^default-options-ht ()
    (loop
       :for kw-name :being :the
       :hash-keys :in (options *keywords*)
       :using (hash-value kw-option-settings)
       :with default-ht = (make-hash-table)
       :do (setf (gethash kw-name default-ht) (second kw-option-settings))
       :finally (return default-ht)))

  (defun kw^section-designators-alist ()
    (hash-table-alist (section-designators *keywords*)))

  (defun kw^markup-alist ()
    (hash-table-alist (markup *keywords*)))

  (defun kw^returns-alist ()
    (hash-table-alist (returns *keywords*)))

  (defun kw^options-alist ()
    (hash-table-alist (options *keywords*)))

  (defun kw^section-designators-names ()
    (hash-table-values
     (section-designators *keywords*)))  

  (defun section-designator? (arg)
    (let ((arg-name (symbol-name? arg)))
      (find-if (curry #'string-equal arg-name)
               (kw^section-designators-names))))

  (defun parse-2valued-keyword (alist lexem lexem-next-args)
    (loop
       :for (name . (value parse-fn))
       :in alist
       :when (string-equal value lexem)
       :do (return (funcall parse-fn lexem-next-args))))

  (defun parse-return-lexem (lexem lexem-next-args)
    (parse-2valued-keyword (kw^returns-alist) lexem lexem-next-args))

  (defun parse-markup-lexem (lexem lexem-next-args)
    (parse-2valued-keyword (kw^markup-alist) lexem lexem-next-args))

  (defun parse-option-lexem (lexem lexem-next-args)
    (loop
       :for (name . (value default parse-fn))
       :in (kw^options-alist)
       :when (string-equal value lexem)
       :do (multiple-value-bind (value-to-use rest-args)
               (funcall parse-fn lexem-next-args)
             (return
               (values (make-instance 'option-token
                                      :name name
                                      :value value-to-use)
                       rest-args)))))

  (defun parse-lexem (lexem lexem-next-args)
    ;; NOTE: special syntax for d here
    (if (and (string-equal (subseq lexem 0 1) "d")
             (not (string-equal (subseq lexem 1) "")))
        (values (make-instance 'delim-token
                               :pattern (subseq lexem 1))
                lexem-next-args)
        (multiple-value-bind (markup-token rest-args)
            (parse-markup-lexem lexem lexem-next-args)
          (if markup-token
              (values markup-token rest-args)
              (multiple-value-bind (option rest-args)
                  (parse-option-lexem lexem lexem-next-args)
                (if option
                    (values option rest-args)
                    (multiple-value-bind (return-token rest-args)
                        (parse-return-lexem lexem lexem-next-args)
                      (when return-token
                        (values
                         (when (print? return-token)
                           (make-instance
                            'regular-token
                            :form (generated-symbol return-token)))
                         rest-args
                         return-token)))))))))

  (defun parse-symbol (sym-name args)
    (let ((lexem-next-args (rest args)))
      (multiple-value-bind (token rest-args return-token)
          (parse-lexem sym-name lexem-next-args)
        (cond (token
               (values token rest-args return-token))
              (return-token
               (values nil rest-args return-token))
              (t
               (values (make-instance 'regular-token
                                      :form (first args))
                       lexem-next-args))))))

  (defun parse-markup (args &optional tokens return-tokens)
    (if (not args)
        (values (reverse tokens) nil return-tokens)
        (aif (symbol-name? (first args))
             (if (section-designator? (first args))
                 (values (reverse tokens) args return-tokens)
                 (multiple-value-bind (token rest-args return-token)
                     (parse-symbol it args)
                   (parse-markup rest-args
                                 (cons-if-truthy token tokens)
                                 (cons-if-truthy return-token return-tokens))))
             (parse-markup (rest args)
                           (cons (make-instance 'regular-token
                                                :form (first args))
                                 tokens)
                           return-tokens))))

  (defun section-slot-name (lexem)
    (dolist (entry (kw^section-designators-alist))
      (destructuring-bind (name . value) entry
        (when (string-equal value lexem)
          (return (intern (symbol-name name) :fmt))))))

  (defclass+ parsed ()
    prefix
    message
    returns
    (options (kw^default-options-ht)))

  (defun parse-sections (args &optional (parsed (make-instance 'parsed)))
    (if (not args)
        parsed
        (multiple-value-bind (slot-name-sym args-to-parse)
            (aif (aand (symbol-name? (first args))
                       (section-slot-name it))
                 (values it (rest args))
                 ;; NOTE: if no section is specified as the first arg
                 ;;       => it's message ('m>') section
                 (values 'message args))
          (multiple-value-bind (token-list rest-args return-tokens)
              (parse-markup args-to-parse)
            (setf (slot-value parsed slot-name-sym)
                  (append (slot-value parsed slot-name-sym) token-list))
            (when return-tokens
              (setf (returns parsed)
                    (append return-tokens (returns parsed))))
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
    (with-slots (prefix message returns options)
        (parse-sections body)
      (multiple-value-bind (redefined-options cleaned-prefix)
          (pick-options prefix options)
        (multiple-value-bind (redefined-options cleaned-message)
            (pick-options message redefined-options)
          (make-instance 'parsed
                         :prefix  cleaned-prefix
                         :message cleaned-message
                         :returns returns
                         :options redefined-options)))))

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
        (with-slots (form) token
          (cond ((stringp form) (%+word-delimiter form))
                ((%stringable? form)
                 (%+word-delimiter (write-to-string form)))
                (t (values (%+word-delimiter format-letter)
                           form)))))))

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

  ) ; eval-when

;; functions that access *counter* are prefixed with c^
(defvar *counter* 0)

(declaim (inline c^get c^reset c^inc dbp-reset-counter))

(defun c^get () *counter*)

(defun c^reset ()
  (setf *counter* 0))

(defun c^inc ()
  (incf *counter*))

(defun dbp-reset-counter ()
  (c^reset))

(defun prepare-clips (no-clip)
  (mappend #'list '(:oneline :upper :middle :lower)
           (if (not no-clip)
               '("• " "┌ " "│ ""└ ")
               '("" "" "" ""))))

(defun prepare-prefix (prefix width cut?)
  (if prefix
      (concatenate 'string
                   (fit-into prefix width :cut? cut?)
                   " ")
      ""))

(defun prepare-counter (no-counter width cut?)
  (if (not no-counter)
      (concatenate 'string
                   (fit-into (write-to-string (c^get)) width :cut? cut?)
                   " ")
      ""))

(defun prepare-stream (stream break)
  (if break (make-string-output-stream) stream))

(defun dbp-print-message (&key prefix message options)
  (when (gethash :reset-counter options) (c^reset))
  (let* ((prefix (prepare-prefix prefix
                                 (gethash :prefix-width options)
                                 (gethash :cut-prefix options)))
         (message (or message ""))
         (clips (prepare-clips (gethash :no-clip options)))
         (break? (gethash :break options))
         (stream (prepare-stream (gethash :stream options) break?))
         (counter (prepare-counter (gethash :no-counter options)
                                   (gethash :counter-width options)
                                   (gethash :cut-counter options)))
         (newlines (loop :for ch :across message :count
                        (char-equal ch #\newline))))
    (if (= newlines 0)
        (format stream "~A~A~A~A"
                (getf clips :oneline) counter prefix message)
        (progn
          (format stream "~A~A~A"
                  (getf clips :upper) counter prefix)
          (loop
             :for ch :across message
             :with i = 0
             :do
               (format stream "~C" ch)
               (when (char-equal ch #\newline)
                 (incf i)
                 (format stream "~A~A~A"
                         (if (= i newlines)
                             (getf clips :lower)
                             (getf clips :middle))
                         counter prefix)))))
    (when (not (gethash :no-end-newline options))
      (format stream "~%"))
    (when break?
      (break "~A" (get-output-stream-string stream)))
    (c^inc)))

(defmacro dbp (&body body)
  "dbp is short for debug print.
It is a macro with a small markup syntax for a better experience
of debugging with debug prints."
  (with-slots (prefix message options returns)
      (parse-body body)
    (multiple-value-bind (prefix-format)
        (translate-tokens prefix options)
      (multiple-value-bind (message-format)
          (translate-tokens message options)
        `(let ,(mapcar
                (lambda (return-token)
                  (list (generated-symbol return-token)
                        (form return-token)))
                returns)
           (dbp-print-message :prefix ,prefix-format
                              :message ,message-format
                              :options ,options)
           (values ,@(mapcar #'generated-symbol returns)))))))
