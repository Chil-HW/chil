(in-package :chil/backends)

;;; Verilog Generator
(defclass verilog-generator (hdl-generator)
  (:documentation "An HDL back-end for generating Verilog."))

(defun chil-sym->verilog-sym (str)
  "Replace characters in STR that are invalid for Verilog symbols."
  ;; TODO: Need a better way to map characters to replace to known-good
  ;; characters for that particular HDL.
  (let ((chars-to-replace (cl-ppcre:create-scanner "-")))
    (cl-ppcre:regex-replace-all chars-to-replace str "_")))

(defmethod generate ((generator verilog-generator) module stream)
  "Generate Verilog for the provided MODULE, outputting the Verilog to STREAM."
  (flet ((module-io->strings (direction args)
           "Generate Verilog argument string based on ARGS, noting the DIRECTION
of the arguments in the string as well.

The two directions supported are the symbols 'input and 'output."
           (assert (member direction '(input output)))
           ;; NOTE: We choose to only support the "wire" type in Verilog, for
           ;; module I/O, the same way Chisel did. Rather, Chisel does not
           ;; specify the type at all, which typically infers "wire".
           (mapcar (lambda (arg)
                     (format 'nil "~a ~a"
                             (string-downcase (symbol-name direction))
                             (chil-sym->verilog-sym arg)))
                   args)))

    (format stream "module ~a " (chil-sym->verilog-sym (chil:module-name module)))
    ;; TODO: Get indentation working
    ;; Module parameters
    ;; NOTE: Parameters only output when at least one parameter is provided.
    (unless (uiop:emptyp (chil:module-parameters module))
      (format stream "#(")
      (format stream "~%~{~a~^,~&~}~&"
              (mapcar (trivia:lambda-match
                        ;; Parameter without default value
                        ((list name)
                         (format 'nil "parameter ~a"
                                 (chil-sym->verilog-sym name)))
                        ;; Parameter with default value
                        ((list name val)
                         (format 'nil "parameter ~a = ~a"
                                 (chil-sym->verilog-sym name)
                                 val))
                        ;; TODO: Raise an error if your module uses parameters
                        ;; incorrectly.
                        (_
                         (error "Modules must have 1 or 2 elements in their parameters.")))
                      (chil:module-parameters module)))
      (format stream ")~&"))
    ;; Module I/O
    (format stream "(")
    (format stream "~%~{~a~^,~&~}~&" ; ~% FORCES a line break!
            (append (module-io->strings 'input (chil:module-inputs module))
                    (module-io->strings 'output (chil:module-outputs module))))
    (format stream ");~&")
    ;; TODO: Actually write a real body
    (format stream "body;~&")
    (format stream "endmodule // ~a" (chil-sym->verilog-sym
                                      (chil:module-name module)))))
