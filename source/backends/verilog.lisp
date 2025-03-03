(defpackage :chil/backends/verilog
  (:use :cl :cl-ppcre :chil/backends)
  (:export #:verilog-identifier-regexp
           #:chil-sym->verilog-sym
           #:verilog-module
           #:verilog-net
           #:*verilog-binops*
           #:+ #:- #:* #:/ #:% #:bit-and #:bit-or
           #:verilog-binop
           #:verilog-assign
           #:codegen))

(in-package :chil/backends/verilog)


;;;
;;; Helpers for converting from Chil to Verilog
;;;

(alexandria:define-constant verilog-identifier-regexp
  '(:sequence
    (:greedy-repetition 1 nil
     (:char-class
      (:range #\a #\z)
      (:range #\A #\Z)
      #\_))
    (:greedy-repetition 0 nil
     (:char-class
      (:range #\a #\z)
      (:range #\A #\Z)
      (:range #\0 #\9)
      #\_ #\$)))
  :test #'equal
  :documentation
  "A cl-ppcre \"parse tree\" to match Verilog identifiers.

Taken from the IEEE Standard for SystemVerilog, IEEE 1800-2023.

Corresponds to the Perl regular expression \"[[:alpha:]_]+[[:alnum:]_$]*\".")

;; Symbol/string conversion should be done at the Chil -> Backend instantiation
;; time. Using this function after the instantiation of a backend Verilog module
;; is effectively a no-op.
(defun chil-sym->verilog-sym (str)
  "Replace characters in STR that are invalid for Verilog symbols."
  ;; TODO: Should we allow users to configure how Chil symbols get replaced with
  ;; a backend-friendly format?
  (let ((chars-to-replace (cl-ppcre:create-scanner "[#%&<=>?@\\^\\-\\/]")))
    (uiop:strcat "GENV_" (cl-ppcre:regex-replace-all chars-to-replace str "_"))))


;;;
;;; Verilog Backend Structure
;;;

(defclass verilog-net (verilog-module)
  ((name
    :reader name
    :initarg :name
    :type string
    :documentation "Name for this net."))
  (:documentation "Verilog nets.

A Verilog net is a structurally \"combinational\" hardware circuit. They can be
just a single wire or a very large combinational net. Sequential elements are
allowed in Verilog nets, but they must behave in a certain way?"))

(defmethod codegen ((net verilog-net) stream)
  (format stream "~a" (name net)))

(defclass verilog-module (chil/backends:backend-hdl)
  ((name
    :reader name
    :initarg :name
    :type string
    :documentation "Verilog module's name.")
   ;; FIXME: Do we actually need to propagate parameter information? The Chil
   ;; level of the language should handle elaborating and resolving all
   ;; parameters, shouldn't it? But what about future interoperability?
   (parameters
    :reader parameters
    :initarg :parameters
    :initform '()
    ;; :type alist-of-string-vals
    :documentation "Parameters that this module should have.")
   (inputs
    :reader inputs
    :initarg :inputs
    :initform '()
    ;; :type list-of-verilog-net
    :documentation "Input wires to this Verilog module.")
   (outputs
    :reader outputs
    :initarg :outputs
    :initform '()
    ;; :type list-of-verilog-net
    :documentation "Output wires from this Verilog module.")
   (body
    :reader body
    :initarg :body
    ;; TODO: Is it legal to have an empty body in a generated Verilog module?
    ;; This may be useful if I want to support arbitrary blackboxes.
    :initform '()
    :type verilog-net
    :documentation "The body of this Verilog module."))
  (:documentation "Verilog-specific backend module."))

(defmethod codegen ((verilog-module verilog-module) stream)
  (flet ((module-io->strings (direction ios)
           "Generate Verilog argument string based on ARGS, noting the DIRECTION
of the arguments in the string as well.

The two directions supported are the symbols 'input and 'output."
           (assert (member direction '(input output)))
           ;; NOTE: We choose to only support the "wire" type in Verilog, for
           ;; module I/O, the same way Chisel did. Rather, Chisel does not
           ;; specify the type at all, which typically infers "wire".
           (mapcar (lambda (io)
                     (format 'nil "~a ~a"
                             (string-downcase (symbol-name direction))
                             (chil-sym->verilog-sym (name io))))
                   ios)))
    (format stream "module ~a " (name verilog-module))
    ;; NOTE: Parameters only output when at least one parameter is provided.
    (unless (uiop:emptyp (parameters verilog-module))
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
                      (parameters verilog-module)))
      (format stream ")~&"))
    ;; Module I/O
    (format stream "(")
    ;; TODO: Get indentation working
    ;; NOTE: We choose to only support the "wire" type in Verilog, for
    ;; module I/O, the same way Chisel did. Rather, Chisel does not
    ;; specify the type at all, which typically infers "wire".
    (format stream "~%~{~a~^,~&~}~&" ; ~% FORCES a line break!
            (append (module-io->strings 'input (inputs verilog-module))
                    (module-io->strings 'output (outputs verilog-module))))
    (format stream ");~&")
    (if (body verilog-module)
        (codegen (body verilog-module) stream)
        (format stream "body;"))
    ;; FIXME: This manual forced newline should not be here.
    (format stream "~%endmodule // ~a" (name verilog-module))))

(defparameter *verilog-binops*
  '(+ - * / % bit-and bit-or bit-xor))
(trivia:defun-ematch binops->verilog (op)
  ('+ "+")
  ('- "-")
  ('* "*")
  ('/ "/")
  ('% "%")
  ('bit-and "&")
  ('bit-or "|")
  ('bit-xor "^"))

(deftype verilog-binops ()
  `(member ,@*verilog-binops*))

;; FIXME: Should this subclass something else (like verilog-net)?
;; Then anything that can drive a combinational net could be used here. However,
;; a binary operation can be used in a sequential net too...
;; There is something to think about here.
;; The LHS & RHS of the binop should also be this abstract type too.
(defclass verilog-binop (verilog-net)
  ((op
    :reader op
    :initarg :op
    :type verilog-binops
    :documentation "Operation to perform")
   (lhs
    :reader lhs
    :initarg :lhs
    :type verilog-net
    :documentation "Left-Hand Side of Binary Operation")
   (rhs
    :reader rhs
    :initarg :rhs
    :type verilog-net
    :documentation "Right-Hand Side of Binary Operation"))
  (:documentation "Verilog's binary operations."))

(defmethod codegen ((binop verilog-binop) stream)
  (codegen (lhs binop) stream)
  (format stream " ~a " (binops->verilog (op binop)))
  (codegen (rhs binop) stream))

(defclass verilog-assign (verilog-net)
  ((target
    :reader target
    :initarg :target
    :type verilog-net
    :documentation "The target of a Verilog \"assign\" statement.")
   (body
    :reader body
    :initarg :body
    :type verilog-net
    :documentation "The body to evaluate in a Verilog \"assign\" statement."))
  (:documentation "Implementation of Verilog's \"assign\" statement."))

(defmethod codegen ((component verilog-assign) stream)
  (format stream "assign ")
  (codegen (target component) stream)
  (format stream " = ")
  (codegen (body component) stream)
  (format stream ";"))
