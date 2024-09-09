(in-package :chil/tests)

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

(defun generate-verilog (module)
  "Helper function to generate Verilog modules. Returns the generated Verilog as
a string."
  (uiop:with-output (str 'nil)
    (chil/backends:generate
     (make-instance 'chil/backends:verilog-generator) module str)))

(define-test generate-verilog-empty-module ()
  (assert-equal "module test_empty (
);
body;
endmodule // test_empty"
                (generate-verilog *test-module-empty*)))

(define-test generate-verilog-module-only-inputs ()
  (assert-equal "module test_inputs (
input addr,
input ready
);
body;
endmodule // test_inputs"
                (generate-verilog *test-module-inputs*)))

(define-test generate-verilog-module-only-outputs ()
  (assert-equal "module test_outputs (
output addr,
output valid
);
body;
endmodule // test_outputs"
                (generate-verilog *test-module-outputs*)))

(define-test generate-verilog-module-inputs-outputs ()
  (assert-equal "module test_inputs_outputs (
input addr,
input valid,
output addr,
output valid
);
body;
endmodule // test_inputs_outputs"
                (generate-verilog *test-module-inputs-outputs*)))

(define-test generate-verilog-module-parameters ()
  (assert-equal "module test_parameters #(
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
