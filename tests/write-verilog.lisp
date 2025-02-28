(defpackage :chil/tests/verilog
  (:use :cl :lisp-unit2 :check-it
        :chil/backends :chil/backends/verilog))

(in-package :chil/tests/verilog)

(defparameter *test-module-empty*
  (chil/backends/verilog:verilog-module "test-empty")
  "CHIL Module for testing that has no inputs, no outputs, and no body.")

(defparameter *test-module-inputs*
  (chil/backends/verilog:verilog-module "test-inputs"
               :inputs '("addr" "ready"))
  "CHIL Module for testing that has only inputs, no outputs, and no body.")

(defparameter *test-module-outputs*
  (chil/backends/verilog:verilog-module "test-outputs"
               :outputs '("addr" "valid"))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defparameter *test-module-inputs-outputs*
  (chil/backends/verilog:verilog-module "test-inputs-outputs"
               :inputs '("addr" "valid")
               :outputs '("addr" "valid"))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defun generate-verilog (vmodule)
  "Helper function to generate Verilog modules. Returns the generated Verilog as
a string."
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

;; TODO: This new output format provides us a way to generate VERILOG modules of
;; arbitrary complexity. This is quite helpful for our dreams of property-based
;; test generation. We will need to couple the Verilog output generation to a
;; Verilog linter to make sure what we generate is actually valid Verilog.
(uiop:with-output (str 'nil)
  (chil/backends/verilog:codegen
   (make-instance 'verilog-assign
                  :target (make-instance 'verilog-net :name "DEV")
                  :body (make-instance
                         ;; TODO: :op is a chance for property-checking test
                         ;; generation.
                         'verilog-binop :op 'bit-or
                         :lhs (make-instance 'verilog-net :name "foo")
                         :rhs (make-instance 'verilog-net :name "BAR")))
   str))

(uiop:with-output (str 'nil)
  (let ((inputs (list (make-instance 'verilog-net :name "valid")
                      (make-instance 'verilog-net :name "addr")))
        (outputs (list (make-instance 'verilog-net :name "valid")
                       (make-instance 'verilog-net :name "data"))))
    (chil/backends/verilog:codegen
     (make-instance 'chil/backends/verilog:verilog-module
                    :name "foo"
                    :inputs inputs
                    :outputs outputs
                    :body (make-instance 'verilog-assign :target (second outputs)
                                                         :body (second inputs)))
     str)))

(uiop:with-output (str 'nil)
  (let* ((inputs (list (make-instance 'verilog-net :name "x")
                       (make-instance 'verilog-net :name "y")))
         (binop (make-instance 'verilog-binop :op 'bit-and
                                              :lhs (first inputs)
                                              :rhs (second inputs)))
         (outputs (list (make-instance 'verilog-net :name "f"))))
    (chil/backends/verilog:codegen
     (make-instance 'chil/backends/verilog:verilog-module
                    :name "foo"
                    :inputs inputs
                    :outputs outputs
                    :body (make-instance 'verilog-assign
                                         :target (first outputs)
                                         :body binop))
     str)))

(uiop:with-output (str 'nil)
  (let* ((inputs (list (make-instance 'verilog-net :name "x")
                       (make-instance 'verilog-net :name "y")))
         (outputs (list (make-instance 'verilog-net :name "f")))
         (binop (make-instance 'verilog-binop :op 'bit-and
                                              :lhs (first inputs)
                                              :rhs (second inputs)))
         (params '(("EMPTY") ("WIDTH" 32))))
    (chil/backends/verilog:codegen
     (make-instance 'chil/backends/verilog:verilog-module
                    :name "foo"
                    :parameters params
                    :inputs inputs
                    :outputs outputs
                    :body (make-instance 'verilog-assign
                                         :target (first outputs)
                                         :body binop))
     str)))
(define-test generate-verilog-assign ()
  (assert-string-equal
   "assign DEV = foo | BAR;"
   (generate-verilog
    (make-instance 'verilog-assign
                   :target (make-instance 'verilog-net :name "DEV")
                   :body (make-instance
                          ;; TODO: :op is a chance for property-checking test
                          ;; generation.
                          'verilog-binop :op 'bit-or
                          :lhs (make-instance 'verilog-net :name "foo")
                          :rhs (make-instance 'verilog-net :name "BAR"))))))

