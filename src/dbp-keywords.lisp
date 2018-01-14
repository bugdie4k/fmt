(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro kw/instance-of (class-sym)
    `(lambda (lexem-next-args)
       (list (make-instance ,class-sym) lexem-next-args)))

  (defun kw/take1-to-string (lexem-next-args)
    (list (princ-to-string (first lexem-next-args))
          (rest lexem-next-args)))

  (defun kw/take1 (lexem-next-args)
    (list (first lexem-next-args)
          (rest lexem-next-args)))

  (defun kw/set-to (value)
    (lambda (lexem-next-args)
      (list value lexem-next-args)))

  (defun gensym? (form &optional (name "G"))
    (unless (or (stringp form)
                (characterp form)
                (numberp form)
                (keywordp form))
      (gensym name)))

  (defun form->regular-token (form)
    (make-instance 'regular-token
                   :form form
                   :generated-symbol (gensym? form "REGULAR")))

  (defun kw/literally (lexem-next-args)
    (list (form->regular-token (first lexem-next-args))
          (rest lexem-next-args)))

  (defun kw/delimiter (lexem-next-args)
    (list (make-instance 'delim-token
                         :pattern (princ-to-string
                                   (first lexem-next-args)))
          (rest lexem-next-args)))

  (defun kw/newline (conditional? lexem-next-args)
    (list (make-instance 'newline-token :conditional? conditional?)
          lexem-next-args))

  (defun form->return-token (form &optional print?)
    (make-instance 'return-token
                   :form form
                   :generated-symbol (gensym? form "RETURNED")
                   :print? print?))

  (defun kw/return (print? lexem-next-args)
    (list (form->return-token (first lexem-next-args) print?)
          (rest lexem-next-args)))

  (defun kw/apply-kw-fns (keywords-ht lexem lexem-next-args)
    (loop
       :for (name . (value parse-fn))
       :in (hash-table-alist keywords-ht)
       :when (string-equal value lexem)
       :return (funcall parse-fn lexem-next-args)))

  (defun kw/options (keywords-ht lexem lexem-next-args)
    (loop
       :for (name . (value default parse-fn))
       :in (hash-table-alist keywords-ht)
       :when (string-equal value lexem)
       :do (destructuring-bind (value-to-use rest-args)
               (funcall parse-fn lexem-next-args)
             (return
               (list (make-instance 'option-token
                                    :name name
                                    :value value-to-use)
                     rest-args)))))

  ;; functions that access *keywords* directly are prefixed with kw^
  (defparameter *keywords*
    (ht
     (:section-designators #'kw/apply-kw-fns
                           (ht
                            (:prefix  "p>" (kw/instance-of 'prefix-section))
                            (:message "m>" (kw/instance-of 'message-section))))
     (:markup #'kw/apply-kw-fns
              (ht
               (:literally           "l"   #'kw/literally)
               (:delimiter           "d"   #'kw/delimiter)
               (:newline             "nl"  (curry #'kw/newline nil))
               (:conditional-newline "cnl" (curry #'kw/newline t))))
     (:returns #'kw/apply-kw-fns
               (ht
                (:return       "r="  (curry #'kw/return nil))
                (:print-return "pr=" (curry #'kw/return t))))
     (:options #'kw/options
               (ht
                (:word-delimiter  "?wd"             " " #'kw/take1-to-string)
                (:format-letter   "?fletter"        "S" #'kw/take1-to-string)
                (:stream          "?s"              t   #'kw/take1)
                (:delimiter-width "?dw"             60  #'kw/take1)
                (:counter-width   "?counter-w"      3   #'kw/take1)
                (:prefix-width    "?prefix-w"       8   #'kw/take1)
                (:cut-counter     "?cut-counter"    nil (kw/set-to t))
                (:cut-prefix      "?cut-prefix"     nil (kw/set-to t))
                (:break           "?break"          nil (kw/set-to t))
                (:reset-counter   "?rsc"            nil (kw/set-to t))
                (:no-end-newline  "?no-end-newline" nil (kw/set-to t))
                (:no-counter      "?no-counter"     nil (kw/set-to t))
                (:no-clip         "?no-clip"        nil (kw/set-to t))))))

  (defun kw^get (&optional section kw)
    (cond ((and section kw)
           (gethash kw (second (gethash section *keywords*))))
          ((and section (not kw))
           (gethash section *keywords*))
          ((and (not section) (not kw))
           *keywords*)))

  (defun kw^default-options-ht ()
    (loop
       :for kw-name :being :the :hash-keys :in (second (kw^get :options))
       :using (hash-value kw-option-settings)
       :with default-ht = (make-hash-table)
       :do (setf (gethash kw-name default-ht) (second kw-option-settings))
       :finally (return default-ht)))

  ) ; eval-when
