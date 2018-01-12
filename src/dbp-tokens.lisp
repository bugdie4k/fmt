(in-package #:fmt)

(eval-when (:compile-toplevel :load-toplevel :execute)

  (defclass+ token ())
  (defclass+ section-token (token))

  (defclass+ prefix-section (section-token))
  (defclass+ message-section (section-token))

  (defclass+ regular-token (token) form generated-symbol)
  (defclass+ newline-token (token) conditional?)
  (defclass+ delim-token (token) pattern)

  (defclass+ return-token (token) form generated-symbol print?)
  (defclass+ option-token (token) name value)

  (defclass+ new-token (token) heh)

  (defmethod print-object ((tok token) stream)
    (format stream ".~A."
            (handler-case (token-designator tok)
              (simple-error (e)
                (declare (ignore e))
                (class-name (class-of tok))))))

  (defgeneric token-designator (tok)
    (:documentation "Return string with textual token representation"))

  (defmethod token-designator ((tok prefix-section)) "P>")
  (defmethod token-designator ((tok message-section)) "M>")

  (defmethod token-designator ((tok regular-token))
    (format nil "REG<~S>" (form tok)))

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
    (format nil "O<~A : ~A>" (name tok) (value tok)))

  ) ; eval-when
