(in-package #:fmt)

(defclass+ token ())
(defclass+ newline-token (token) conditional?)
(defclass+ delim-token (token) pattern)
(defclass+ return-token (token) form print?)

(defun token? (arg)
  (typep arg 'token))

(defun parse-literally (next-args)
  (values (first next-args)
          (rest next-args)))

(defun parse-delimiter (next-args)
  (values (make-instance 'delim-token :pattern
                         (symbol-name (first next-args)))
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

(defmacro defkeywords (&rest definitions-by-type)
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

(defkeywords
    (:section-designators
     (:prefix "p>")
     (:message "m>"))
    (:markup
     (:literally "l" #'parse-literally)
     (:delimiter "d" #'parse-delimiter)
     (:newline "nl" (curry #'parse-newline nil))
     (:conditional-newline "cnl" (curry #'parse-newline t))
     (:return "r" (curry #'parse-return nil))
     (:print-return "pr" (curry #'parse-return t))))

(defun symbol-name? (arg)
  (and (symbolp arg) (symbol-name arg)))

(defun section-designator? (arg)
  (let ((arg-name (symbol-name? arg)))
    (find-if (curry #'string-equal arg-name)
             (hash-table-values
              (section-designators *keywords*)))))

(defun parse-markup-lexem (lexem next-args)
  (dolist (entry (hash-table-alist (markup *keywords*)))
    (destructuring-bind (name . (value parse-fn)) entry
      (declare (ignore name))
      (when (string-equal value lexem)
        (return (funcall parse-fn next-args))))))

(defun parse-markup (args)
  (labels ((%parse-lexem (lexem args)
             (let* ((next-args (rest args)))
               (if (string-equal (subseq lexem 0 1) "d")
                   ;; NOTE: special syntax for d
                   (values (make-instance 'delim-token
                                          :pattern (subseq lexem 1))
                           next-args)
                   (multiple-value-bind (markup-token? rest-args)
                       (parse-markup-lexem lexem next-args)
                     (if markup-token?
                         (values markup-token? rest-args)
                         (values (first args)
                                 next-args))))))
           (%parse (args &optional tokens)
             (if (not args)
                 (values tokens nil)
                 (aif (symbol-name? (first args))
                      (if (section-designator? (first args))
                          (values tokens args)
                          (multiple-value-bind (token rest-args)
                              (%parse-lexem it args)
                            (%parse rest-args (cons token tokens))))
                      (%parse (rest args)
                              (cons (first args)
                                    tokens))))))
    (multiple-value-bind (rev-token-list rest-args)
        (%parse args)
      (values (reverse rev-token-list) rest-args))))

(defun section-slot-name (lexem)  
  (dolist (entry (hash-table-alist (section-designators *keywords*)))
    (destructuring-bind (name . value) entry      
      (when (string-equal value lexem)
        (return (intern (symbol-name name) :fmt))))))

(defclass+ parsed-sections ()
  prefix message)

(defun parse-sections (args
                       &optional (parsed (make-instance 'parsed-sections)))  
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
