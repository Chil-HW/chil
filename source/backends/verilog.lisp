(in-package :chil/backends)

;;; Verilog Generator
(defclass verilog-generator (hdl-generator)
  ())

(defun chil-sym->verilog-sym (str)
  "Replace characters in STR that are invalid for Verilog symbols."
  ;; TODO: Need a better way to map characters to replace to known-good
  ;; characters for that particular HDL.
  (let ((chars-to-replace (cl-ppcre:create-scanner "-")))
    (cl-ppcre:regex-replace-all chars-to-replace str "_")))

(defmethod generate ((generator verilog-generator) module stream)
  "Generate Verilog for the provided MODULE."
  (flet ((module-io->strings (direction args)
           "Generate Verilog argument string based on ARGS, noting the DIRECTION
of the arguments in the string as well.

The two directions supported are the symbols 'input and 'output."
           (mapcar (lambda (arg)
                     (format 'nil "~a type ~a"
                             (string-downcase (symbol-name direction))
                             (chil-sym->verilog-sym arg)))
                     args)))

  (uiop:strcat
   (format stream "module ~a" (chil-sym->verilog-sym (chil:module-name module)))
   (format stream "(")
   (format stream "~%~{~a~^,~&~}~&" ; ~% FORCES a line break!
           (append (module-io->strings 'input (chil:module-inputs module))
                   (module-io->strings 'output (chil:module-outputs module))))
   (format stream ");~&")
   ;; TODO: Actually write a real body
   ;; TODO: Get indentation working
   (format stream "body;~&")
   (format stream "endmodule // ~a" (chil-sym->verilog-sym
                                     (chil:module-name module))))))
