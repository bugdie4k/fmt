(in-package #:fmt)

(defmacro aif (test-form then-form &optional else-form)
  "Anaphoric if: use `it' in then-form, else-form to
   refer to result of the test-form."
  `(let ((it ,test-form))
     (declare (ignorable it))
     (if it ,then-form ,else-form)))

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
