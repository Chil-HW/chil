(defpackage :chil/tests/verilog
  (:use :cl :lisp-unit2 :check-it
        :chil/backends :chil/backends/verilog))

(in-package :chil/tests/verilog)


;;;
;;; Helpers for Verilog unit tests
;;;

(defun generate-verilog (vmodule)
  "Helper function to generate Verilog modules. Returns the generated Verilog as
a string."
  (string-trim '(#\Newline #\Space)
               (uiop:with-output (str 'nil)
                 (chil/backends/verilog:codegen vmodule str))))


;;;
;;; Tests for various utilities used by the Verilog backend.
;;;

(define-test chil->verilog-symbol-conversion ()
  ;; Underscores are a valid Chil symbol
  (let ((chil-symbol "13__"))
    (assert-string/= chil-symbol
     (cl-ppcre:scan-to-strings chil/backends/verilog:verilog-identifier-regexp
                               (chil-sym->verilog-sym chil-symbol))))
  ;; If the replaced string is parseable into a Verilog symbol (which means the
  ;; original conversion and the scanned version are the same), then the
  ;; replacement function is correct.
  (assert-true
   (check-it:check-it (check-it:generator (chil/tests:chil-symbol-generator))
    (lambda (chil-id)
      ;; TODO: We should impose a character limit on identifiers. The Verilog
      ;; spec says at least 1024-long identifiers should be allowed.
      ;; (assert-string= (chil-sym->verilog-sym chil-id)
      (string= (chil-sym->verilog-sym chil-id)
       (multiple-value-bind (substr-match regs)
           (cl-ppcre:scan-to-strings chil/backends/verilog:verilog-identifier-regexp
                                     (chil-sym->verilog-sym chil-id))
         (declare (ignore regs))
         substr-match))))))


;;;
;;; Begin Verilog codegen unit tests
;;;

(defparameter *test-module-empty*
  (make-instance 'verilog-module
                 :name (make-exportable-verilog-symbol :name "test-empty"))
  "CHIL Module for testing that has no inputs, no outputs, and no body.")

(defparameter *test-module-inputs*
  (let ((inputs (list (make-exportable-verilog-symbol :name "valid")
                      (make-exportable-verilog-symbol :name "addr"))))
    (make-instance 'verilog-module
                   :name (make-exportable-verilog-symbol :name "test-inputs")
                   :inputs inputs))
  "CHIL Module for testing that has only inputs, no outputs, and no body.")

(defparameter *test-module-outputs*
  (let ((outputs (list (make-exportable-verilog-symbol :name "ready")
                       (make-exportable-verilog-symbol :name "data"))))
    (make-instance 'verilog-module
                   :name (make-exportable-verilog-symbol :name "test_outputs")
                   :outputs outputs))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defparameter *test-module-inputs-outputs*
  (let ((inputs (list (make-exportable-verilog-symbol :name "valid")
                      (make-exportable-verilog-symbol :name "addr")))
        (outputs (list (make-exportable-verilog-symbol :name "ready")
                       (make-exportable-verilog-symbol :name "data"))))
    (make-instance 'verilog-module
                   :name (make-exportable-verilog-symbol :name "test-inputs-outputs")
                   :inputs inputs
                   :outputs outputs))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(define-test generate-verilog-empty-module ()
  (assert-string-equal
   "module test_empty (
);
endmodule // test_empty"
   (generate-verilog *test-module-empty*)))

(define-test generate-verilog-module-only-inputs ()
  (assert-string-equal
   "module test_inputs (
input valid,
input addr
);
endmodule // test_inputs"
   (generate-verilog *test-module-inputs*)))

(define-test generate-verilog-module-only-outputs ()
  (assert-string-equal
   "module test_outputs (
output ready,
output data
);
endmodule // test_outputs"
   (generate-verilog *test-module-outputs*)))

(define-test generate-verilog-module-inputs-outputs ()
  (assert-string-equal
   "module test_inputs_outputs (
input valid,
input addr,
output ready,
output data
);
endmodule // test_inputs_outputs"
   (generate-verilog *test-module-inputs-outputs*)))

(define-test generate-verilog-module-parameters ()
  (assert-string-equal
   "module test_parameters #(
parameter EMPTY,
parameter IN_WIDTH = 32,
parameter out_width = 16
)
(
input valid,
input addr,
output ready,
output data
);
endmodule // test_parameters"
   (let ((params `((,(make-exportable-verilog-symbol :name "EMPTY"))
                   (,(make-exportable-verilog-symbol :name "IN-WIDTH") 32)
                   (,(make-exportable-verilog-symbol :name "out_width") 16)))
         (inputs (list (make-exportable-verilog-symbol :name "valid")
                       (make-exportable-verilog-symbol :name "addr")))
         (outputs (list (make-exportable-verilog-symbol :name "ready")
                        (make-exportable-verilog-symbol :name "data"))))

     (generate-verilog
      (make-instance 'verilog-module
                     :name (make-exportable-verilog-symbol :name "test-parameters")
                     :parameters params
                     :inputs inputs
                     :outputs outputs)))))

(define-test generate-verilog-invalid-parameters ()
  (assert-error 'simple-error
                (let ((params `((,(make-exportable-verilog-symbol :name "IN_WIDTH") 32)
                                (,(make-exportable-verilog-symbol :name "failing") 16 2)))
                      (inputs (list (make-exportable-verilog-symbol :name "valid")
                                    (make-exportable-verilog-symbol :name "addr")))
                      (outputs (list (make-exportable-verilog-symbol :name "ready")
                                     (make-exportable-verilog-symbol :name "data"))))
                  (generate-verilog
                   (make-instance 'verilog-module
                                  :name (make-exportable-verilog-symbol :name "invalid-parameters")
                                  :parameters params
                                  :inputs inputs
                                  :outputs outputs)))))

(define-test generate-verilog-assign ()
  (assert-string-equal
   "assign DEV = foo;"
   (generate-verilog
    (make-instance
     'verilog-assign
     :target (make-instance
              'verilog-net
              :name (make-exportable-verilog-symbol :name "DEV"))
     :body (make-instance
            'verilog-net
            :name (make-exportable-verilog-symbol :name "foo"))))))

(defparameter *test-module-body*
  (let ((inputs (list (make-exportable-verilog-symbol :name "a")))
        (outputs (list (make-exportable-verilog-symbol :name "F"))))
    (make-instance
     'verilog-module
     :name (make-exportable-verilog-symbol :name "test-body")
     :inputs inputs
     :outputs outputs
     :body (list
            (make-instance
             'verilog-assign
             :target (make-instance
                      'verilog-net
                      :name (first outputs))
             :body (make-instance
                    'verilog-net
                    :name (first inputs))))))
  "CHIL Module for testing that is a complete module, but unparameterized.")

(define-test generate-verilog-body ()
  (assert-string-equal
   "module test_body (
input a,
output F
);
assign F = a;
endmodule // test_body"
   (generate-verilog *test-module-body*)))

;; TODO: This new output format provides us a way to generate VERILOG modules of
;; arbitrary complexity. This is quite helpful for our dreams of property-based
;; test generation. We will need to couple the Verilog output generation to a
;; Verilog linter to make sure what we generate is actually valid Verilog.
(define-test generate-verilog-binop ()
  (assert-string-equal
   "foo ^ BAR"
   (generate-verilog
    (make-instance
     ;; TODO: :op is another chance for property-checking test generation.
     'verilog-binop
     :op 'bit-xor
     :lhs (make-instance
           'verilog-net
           :name (make-exportable-verilog-symbol :name "foo"))
     :rhs (make-instance
           'verilog-net
           :name (make-exportable-verilog-symbol :name "BAR"))))))

(define-test generate-verilog-assign-binop ()
  (assert-string-equal
   "assign DEV = foo | BAR;"
   (generate-verilog
    (make-instance
     'verilog-assign
     :target (make-instance
              'verilog-net
              :name (make-exportable-verilog-symbol :name "DEV"))
     :body (make-instance
            ;; TODO: :op is a chance for property-checking test generation.
            'verilog-binop
            :op 'bit-or
            :lhs (make-instance
                  'verilog-net
                  :name (make-exportable-verilog-symbol :name "foo"))
            :rhs (make-instance
                  'verilog-net
                  :name (make-exportable-verilog-symbol :name "BAR")))))))

(defparameter *combinatorial-passthrough-module*
  (let ((inputs (list (make-exportable-verilog-symbol :name "x")))
        (outputs (list (make-exportable-verilog-symbol :name "f"))))
    (make-instance
     'verilog-module
     :name (make-exportable-verilog-symbol :name "passthrough")
     :inputs inputs
     :outputs outputs
     :body (list
            (make-instance
            'verilog-assign
            :target (make-instance 'verilog-net :name (first outputs))
            :body (make-instance 'verilog-net :name (first inputs))))))
  "A simple combinatorial Verilog module that just passes its input argument
directly as its output.
NOTE: This module has NO buffering!")

(define-test passthrough-module ()
  (assert-string-equal
   "module passthrough (
input x,
output f
);
assign f = x;
endmodule // passthrough"
   (generate-verilog *combinatorial-passthrough-module*)))

(defparameter *combinatorial-binop-module*
  (let* ((inputs (list (make-exportable-verilog-symbol :name "x")
                       (make-exportable-verilog-symbol :name "y")))
         (outputs (list (make-exportable-verilog-symbol :name "f")))
         (binop (make-instance
                 'verilog-binop :op '+
                 :lhs (make-instance 'verilog-net :name (first inputs))
                 :rhs (make-instance 'verilog-net :name (second inputs)))))
    (make-instance
     'verilog-module
     :name (make-exportable-verilog-symbol :name "comb-binop")
     :inputs inputs
     :outputs outputs
     :body (list
            (make-instance
             'verilog-assign
             :target (make-instance
                      'verilog-net :name (first outputs))
             :body binop))))
  "A simple purely-combinatorial Verilog module containing what behavioral
Verilog considers a purely-combinational binary operator.")

(define-test combinatorial-binop-module ()
  (assert-string-equal
   "module comb_binop (
input x,
input y,
output f
);
assign f = x + y;
endmodule // comb_binop"
   (generate-verilog *combinatorial-binop-module*)))
