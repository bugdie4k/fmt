(in-package #:fmt)

;; functions that access *counter* directly are prefixed with c^
(defvar *counter* 0)

(declaim (inline c^reset c^inc dbp-reset-counter))

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

(defun c^prepare-counter (no-counter width cut?)
  (if (not no-counter)
      (concatenate 'string
                   (fit-into (write-to-string *counter*) width :cut? cut?)
                   " ")
      ""))

(defun prepare-stream (stream break)
  (if (or (not stream) break) (make-string-output-stream) stream))

(defun dbp-print-message (&key prefix message stream options)
  (when (gethash :reset-counter options) (c^reset))
  (let* ((prefix (prepare-prefix prefix
                                 (gethash :prefix-width options)
                                 (gethash :cut-prefix options)))
         (message (or message ""))
         (clips (prepare-clips (gethash :no-clip options)))
         (break? (gethash :break options))
         (stream-to-use (prepare-stream stream break?))
         (counter (c^prepare-counter (gethash :no-counter options)
                                     (gethash :counter-width options)
                                     (gethash :cut-counter options)))
         (newlines (loop :for ch :across message :count
                        (char-equal ch #\newline))))
    (if (= newlines 0)
        (format stream-to-use "~A~A~A~A"
                (getf clips :oneline) counter prefix message)
        (progn
          (format stream-to-use "~A~A~A"
                  (getf clips :upper) counter prefix)
          (loop
             :for ch :across message
             :with i = 0
             :do
               (format stream-to-use "~C" ch)
               (when (char-equal ch #\newline)
                 (incf i)
                 (format stream-to-use "~A~A~A"
                         (if (= i newlines)
                             (getf clips :lower)
                             (getf clips :middle))
                         counter prefix)))))
    (when (not (gethash :no-end-newline options))
      (format stream-to-use "~%"))
    (c^inc)
    (if break?
        (break "~A" (get-output-stream-string stream-to-use))
        (unless stream
          (get-output-stream-string stream-to-use)))))

(defmacro dbp (&body body)
  "dbp is short for debug print.
It is a macro with a small markup syntax for a better experience
of debugging with debug prints."
  (mv-let* (((prefix message eval-list returns options)
             (parser (lexer body)))
            ((prefix-format)
             (translate-tokens prefix options))
            ((message-format)
             (translate-tokens message options))            
            (dbp-print-message-form
             `(dbp-print-message :prefix ,prefix-format
                                 :message ,message-format
                                 :stream ,(gethash :stream options)
                                 :options ,options))
            (values-list
             (mapcar
              (lambda (return-token)
                (aif (generated-symbol return-token)
                     it
                     (form return-token)))
              returns))
            (body-forms
             (if (gethash :stream options)
                 `(,dbp-print-message-form
                   (values ,@values-list))
                 `((values
                    ,@(cons dbp-print-message-form
                            values-list))))))
    `(let ,eval-list
       ,@body-forms)))
