(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defmacro aif (test-form then-form &optional else-form)
    "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."
    `(let ((it ,test-form))
       (declare (ignorable it))
       (if it ,then-form ,else-form)))

  (defmacro awhen (test-form &body body)
    "Anaphoric when: use `it' in body to
   refer to result of the test-form."
    `(aif ,test-form
          (progn ,@body)))

  (defmacro aand (&rest args)
    "Anaphoric and: use `it' to refer to result of
  previous form evaluation."
    (cond ((null args) t)
          ((null (cdr args)) (car args))
          (t `(aif ,(car args) (aand ,@(cdr args))))))

  (defmacro defclass+ (name direct-superclasses &rest direct-slots)
    "Simpler class definition.
Defining slots you may specify initform as the second el,
and type as third."
    (labels ((%get-slot-def (sym &optional (initform nil))
               (list sym :accessor (intern (symbol-name sym))
                     :initarg (intern (symbol-name sym) "KEYWORD")
                     :initform initform)))
      `(defclass ,name ,direct-superclasses
         ,(when direct-slots
            (mapcar (lambda (slot-def)
                      (if (symbolp slot-def)
                          (%get-slot-def slot-def)
                          (let* ((slotname (first slot-def))
                                 (res-slot-def
                                  (%get-slot-def slotname (second slot-def)))
                                 (type? (third slot-def)))
                            (if type? (append res-slot-def `(:type ,type?))
                                res-slot-def))))
                    direct-slots)))))

  (defun string+ (&rest strings)
    (with-output-to-string (s)
      (dolist (str strings)
        (loop
           :for ch :across str
           :do (format s "~C" ch)))))

  (defun fit-into (string width &key (with-char #\space) (cut? t) (widen? t))
    (let ((len (length string)))
      (cond ((> len width)
             (if cut?
                 (subseq string 0 width)
                 string))
            ((< len width)
             (if widen?
                 (concatenate 'string string
                              (make-string (- width len)
                                           :initial-element with-char))
                 string))
            (t string))))

  (defun symbol-name? (arg)
    (and (symbolp arg) (symbol-name arg)))

  (defun ht-aux (defs)
    (loop
       :for def :in defs
       :with ht = (make-hash-table)
       :do (setf (gethash (first def) ht) (rest def))
       :finally (return ht)))

  (defun list-lists (ls)
    (awhen (first ls)
      (cons
       (if (listp it) `(list ,@it) it)
       (list-lists (rest ls)))))

  (defmacro ht (&rest defs)
    `(ht-aux (list ,@(list-lists defs))))

  (defmacro mv-let* (mv-let-list &body body)
    "(mv-let* ((v form)
               ((v v) form))
      body)"
    (labels ((%vars (vars) (if (listp vars) vars (list vars)))
             (%expand-mv-let* (mv-let-list)
               (let ((mv-let1 (pop mv-let-list)))
                 (if mv-let-list
                     `(multiple-value-bind ,(%vars (first mv-let1))
                          ,(second mv-let1)
                        ,(%expand-mv-let* mv-let-list))
                     `(multiple-value-bind ,(%vars (first mv-let1))
                          ,(second mv-let1)
                        ,@body)))))
      (%expand-mv-let* mv-let-list)))

  ) ; eval-when
