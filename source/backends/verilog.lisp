(in-package :chil/backends)


(defun chil-sym->verilog-sym (str)
  "Replace characters in STR that are invalid for Verilog symbols."
  ;; TODO: Need a better way to map characters to replace to known-good
  ;; characters for that particular HDL.
  (let ((chars-to-replace (cl-ppcre:create-scanner "-")))
    (cl-ppcre:regex-replace-all chars-to-replace str "_")))

(defun generate-and (form)
  (trivia:match form
    ;; FIXME: Matching should require the symbol 'and in the list!
    ;; FIXME: and match should allow for unlimited arguments, like Lisps do.
    ((list _ lhs rhs)
      (format 'nil "~{~a~^ & ~}" (list lhs rhs)))))

(defun generate-assign (form)
  (trivia:match form
    ;; FIXME: Matching should require the symbol 'assign in the list!
    ((list _ res rhs)
     ;; (format 'nil "assign ~a = ~a" res (generate-body rhs))
     (format 'nil "assign ~a = ~a;" res (generate-and rhs)))
    (nil
     (format 'nil "body;"))))

;; TODO: We want to create something akin to a dispatch table
(defmethod generate-body (body-list)
  (if (not (eql body-list 'nil))
      (format 'nil "~{~a~^~&~}"
              (mapcar #'generate-assign body-list))
      (format 'nil "body;")))

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
    (format stream "~a~&" (generate-body (chil:module-body module)))
    (format stream "endmodule // ~a" (chil-sym->verilog-sym
                                      (chil:module-name module)))))
