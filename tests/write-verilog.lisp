(defpackage :chil/tests/verilog
  (:use :cl :lisp-unit2 :check-it
        :chil/backends :chil/backends/verilog))

(in-package :chil/tests/verilog)

(defparameter *test-module-empty* (chil:module "test-empty")
  "CHIL Module for testing that has no inputs, no outputs, and no body.")

(defparameter *test-module-inputs*
  (chil:module "test-inputs"
               :inputs '("addr" "ready"))
  "CHIL Module for testing that has only inputs, no outputs, and no body.")

(defparameter *test-module-outputs*
  (chil:module "test-outputs"
               :outputs '("addr" "valid"))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defparameter *test-module-inputs-outputs*
  (chil:module "test-inputs-outputs"
               :inputs '("addr" "valid")
               :outputs '("addr" "valid"))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defun generate-verilog (vmodule)
  "Helper function to generate Verilog modules. Returns the generated Verilog as
a string.

This creates a fresh Verilog generator on every invocation. This allows each
unit test to potentially modify the generator without polluting other tests."
  (uiop:with-output (str 'nil)
    (chil/backends/verilog:codegen vmodule str)))

(define-test generate-verilog-empty-module ()
  (assert-string-equal "module test_empty (
);
body;
endmodule // test_empty"
                (generate-verilog *test-module-empty*)))

(define-test generate-verilog-module-only-inputs ()
  (assert-string-equal "module test_inputs (
input addr,
input ready
);
body;
endmodule // test_inputs"
                (generate-verilog *test-module-inputs*)))

(define-test generate-verilog-module-only-outputs ()
  (assert-string-equal "module test_outputs (
output addr,
output valid
);
body;
endmodule // test_outputs"
                (generate-verilog *test-module-outputs*)))

(define-test generate-verilog-module-inputs-outputs ()
  (assert-string-equal "module test_inputs_outputs (
input addr,
input valid,
output addr,
output valid
);
body;
endmodule // test_inputs_outputs"
                (generate-verilog *test-module-inputs-outputs*)))

(define-test generate-verilog-module-parameters ()
  (assert-string-equal "module test_parameters #(
parameter EMPTY,
parameter IN_WIDTH = 32,
parameter out_width = 16
)
(
input addr,
input valid,
output addr,
output valid
);
body;
endmodule // test_parameters"
                (generate-verilog
                 (chil:module "test-parameters"
                        :parameters '(("EMPTY") ("IN_WIDTH" 32) ("out-width" 16))
                        :inputs '("addr" "valid")
                        :outputs '("addr" "valid")))))

(define-test generate-verilog-invalid-parameters ()
  (assert-error 'simple-error
    (generate-verilog
     (chil:module "invalid-parameters"
                  :parameters '(("IN_WIDTH" 32) ("failing" 16 2))
                  :inputs '("addr" "valid")
                  :outputs '("addr" "valid")))))

(defparameter *test-module-body*
  (chil:module "test-body"
               :inputs '("a" "b")
               :outputs '("f")
               :body '((assign f (and a b))))
  "CHIL Module for testing that is a complete module, but unparameterized.")

(define-test generate-verilog-body ()
  (assert-string-equal "module test_body (
input a,
input b,
output f
);
assign F = A & B;
endmodule // test_body"
                (generate-verilog *test-module-body*)))
