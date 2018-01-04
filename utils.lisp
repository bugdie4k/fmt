(in-package #:fmt)

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

(defgeneric traverse-slots (obj fn)
  (:documentation
   "Use fn function on each slot.
'fn' must take two parameters: slot-name and slot-value."))

(defmethod traverse-slots (obj fn)
  (labels ((%traverse-slots (slots-lst)
             (when slots-lst
               (let* ((slot (car slots-lst))
                      (def-name (sb-mop:slot-definition-name slot))
                      (name (symbol-name def-name))
                      (value (slot-value obj def-name)))
                 (funcall fn name value)
                 (%traverse-slots (cdr slots-lst))))))
    (%traverse-slots (sb-mop:class-slots (class-of obj)))))

(defgeneric pprint-object (obj &optional stream)
  (:documentation
   "Pretty printer for objects.
Prints all slots with format 'SLOT-NAME: SLOT-VALUE'"))

(defmethod pprint-object (obj &optional (stream t))
  (format stream
          (with-output-to-string (s)
            (traverse-slots
             obj
             (lambda (name val)
               (format s "~A: ~S~%" name val))))))

(defun string+ (&rest strings)
  (format nil "~{~A~}" strings))
