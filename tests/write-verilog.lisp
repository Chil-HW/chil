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
  "Helper function to generate verilog modules."
  (chil/backends:generate (make-instance 'chil/backends:verilog-generator) module nil))

(define-test generate-verilog-empty-module ()
  (assert-equal "module test-empty(
)
body;
endmodule // test-empty"
                (generate-verilog *test-module-empty*)))

(define-test generate-verilog-module-only-inputs ()
  (assert-equal "module test-inputs(
input type addr,
input type ready,
)
body;
endmodule // test-inputs"
                (generate-verilog *test-module-inputs*)))

(define-test generate-verilog-module-only-outputs ()
  (assert-equal "module test-outputs(
output type addr,
output type valid,
)
body;
endmodule // test-outputs"
                (generate-verilog *test-module-outputs*)))

(define-test generate-verilog-module-inputs-outputs ()
  (assert-equal "module test-inputs-outputs(
input type addr,
input type valid,
output type addr,
output type valid,
)
body;
endmodule // test-inputs-outputs"
                (generate-verilog *test-module-inputs-outputs*)))
