(defpackage :chil/backends/vhdl
  (:use :cl :chil/backends)
  (:export #:chil-sym->vhdl-sym
           #:vhdl-module
           #:vhdl-net))

(in-package :chil/backends/vhdl)


;;;
;;; Helpers for converting from Chil to VHDL
;;;

;; Symbol/string conversion should be done at the Chil -> Backend instantiation
;; time. Using this function on a VHDL string, after the instantiation of a
;; backend VHDL module, is effectively a no-op.
;; TODO: This function is ripe for property-based testing!
(defun chil-sym->vhdl-sym (str)
  "Replace characters in STR that are invalid for VHDL symbols."
  ;; TODO: Need a better way to map characters to replace to known-good
  ;; characters for that particular HDL.
  (let ((chars-to-replace (cl-ppcre:create-scanner "-")))
    (cl-ppcre:regex-replace-all chars-to-replace str "_")))


;;;
;;; VHDL Backend Structure
;;;

;; TODO: deftype a string type for Verilog symbols when strings?
;; (deftype vhdl-symbol (chil-str)
;;   `(something ,chil-str))
;; TODO: Need to subtype vhdl-symbol under string because we want the ability
;; to give a VHDL symbol to any string function that Common Lisp provides and
;; do the sensible thing.

(defclass vhdl-net (vhdl-module)
  ((name
    :reader name
    :initarg :name
    :type string
    :documentation "Name for this net."))
  (:documentation "VHDL nets."))

(defmethod codegen ((net vhdl-net) stream)
  (format stream "~a" (name net)))

(defclass vhdl-module (chil/backends:backend-hdl)
  ((name
    :reader name
    :initarg :name
    :type string
    :documentation "VHDL module's name.")
   (inputs
    :reader inputs
    :initarg :inputs
    :initform '()
    ;; :type list-of-vhdl-net
    :documentation "Input wires to this VHDL module.")
   (outputs
    :reader outputs
    :initarg :outputs
    :initform '()
    ;; :type list-of-vhdl-net
    :documentation "Output wires from this VHDL module."))
  (:documentation "VHDL-specific backend module."))

(defmethod codegen ((vmodule vhdl-module) stream)
  "Generate VHDL for the provided VMODULE, outputting the VHDL to STREAM."
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
            (chil-sym->vhdl-sym (name vmodule)))
    ;; TODO: Get indentation working!
    (format stream "port (~{~a~^;~&~});~&"
            (append (mapcar (lambda (io) (module-io->string 'in (name io)))
                            (inputs vmodule))
                    (mapcar (lambda (io) (module-io->string 'out (name io)))
                            (outputs vmodule))))
    (format stream "end entity ~a;~%~%"
            (chil-sym->vhdl-sym (name vmodule)))
    ;; Now we write out an architecture
    (format stream "architecture rtl of ~a is~&"
            (chil-sym->vhdl-sym (name vmodule)))
    (format stream "begin~&")
    ;; TODO: Get indentation working.
    (format stream "body;~%")
    (format stream "end architecture rtl;")))
