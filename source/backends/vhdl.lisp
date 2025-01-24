(in-package :chil/backends)

;;; VHDL Generator
(defclass vhdl-generator (hdl-generator)
  ()
  (:documentation "An HDL back-end for generating VHDL."))

(defun chil-sym->vhdl-sym (str)
  "Replace characters in STR that are invalid for VHDL symbols."
  ;; TODO: Need a better way to map characters to replace to known-good
  ;; characters for that particular HDL.
  (let ((chars-to-replace (cl-ppcre:create-scanner "-")))
    (cl-ppcre:regex-replace-all chars-to-replace str "_")))

(defmethod generate ((generator vhdl-generator) module stream)
  "Generate VHDL for the provided MODULE, outputting the VHDL to STREAM."
  (flet ((module-io->string (direction arg)
           (assert (member direction '(in out)))
           (format 'nil "~a : ~a std_logic" arg
                   (string-downcase (symbol-name direction))))
         ;; FIXME: Can we not nest function definitions in flet?
         ;; (module-io->strings (dir ios)
         ;;   (mapcar (lambda (io) (module-io->string dir io)) ios))
         )
    ;; Module declaration
    (format stream "entity ~a is~%"
            (chil-sym->vhdl-sym (chil:module-name module)))
    ;; TODO: Get indentation working!
    (format stream "port (~{~a~^;~&~});~&"
            (append (mapcar (lambda (io) (module-io->string 'in io))
                            (chil:module-inputs module))
                    (mapcar (lambda (io) (module-io->string 'out io))
                            (chil:module-outputs module))))
    (format stream "end entity ~a;~%~%"
            (chil-sym->vhdl-sym (chil:module-name module)))
    ;; Now we write out an architecture
    (format stream "architecture rtl of ~a is~&"
            (chil-sym->vhdl-sym (chil:module-name module)))
    (format stream "begin~&")
    ;; TODO: Get indentation working.
    (format stream "body;~%")
    (format stream "end architecture rtl;")))
