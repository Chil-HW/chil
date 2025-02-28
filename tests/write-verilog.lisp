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
  (uiop:with-output (str 'nil)
    (chil/backends/verilog:codegen vmodule str)))


;;;
;;; Begin Verilog codegen unit tests
;;;

(defparameter *test-module-empty*
  (make-instance 'verilog-module
                 :name (chil-sym->verilog-sym "test-empty"))
  "CHIL Module for testing that has no inputs, no outputs, and no body.")

(defparameter *test-module-inputs*
  (let ((inputs (list (make-instance 'verilog-net :name "valid")
                      (make-instance 'verilog-net :name "addr"))))
    (make-instance 'verilog-module
                   :name (chil-sym->verilog-sym "test-inputs")
                   :inputs inputs))
  "CHIL Module for testing that has only inputs, no outputs, and no body.")

(defparameter *test-module-outputs*
  (let ((outputs (list (make-instance 'verilog-net :name "ready")
                       (make-instance 'verilog-net :name "data"))))
    (make-instance 'verilog-module
                   :name (chil-sym->verilog-sym "test-outputs")
                   :outputs outputs))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defparameter *test-module-inputs-outputs*
  (let ((inputs (list (make-instance 'verilog-net :name "valid")
                      (make-instance 'verilog-net :name "addr")))
        (outputs (list (make-instance 'verilog-net :name "ready")
                       (make-instance 'verilog-net :name "data"))))
    (make-instance 'verilog-module
                   :name (chil-sym->verilog-sym "test-inputs-outputs")
                   :inputs inputs
                   :outputs outputs))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(define-test generate-verilog-empty-module ()
  (assert-string-equal "module test_empty (
);
body;
endmodule // test_empty"
                (generate-verilog *test-module-empty*)))

(define-test generate-verilog-module-only-inputs ()
  (assert-string-equal "module test_inputs (
input valid,
input addr
);
body;
endmodule // test_inputs"
                (generate-verilog *test-module-inputs*)))

(define-test generate-verilog-module-only-outputs ()
  (assert-string-equal "module test_outputs (
output ready,
output data
);
body;
endmodule // test_outputs"
                (generate-verilog *test-module-outputs*)))

(define-test generate-verilog-module-inputs-outputs ()
  (assert-string-equal "module test_inputs_outputs (
input valid,
input addr,
output ready,
output data
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
input valid,
input addr,
output ready,
output data
);
body;
endmodule // test_parameters"
                       (let ((inputs (list (make-instance 'verilog-net :name "valid")
                                           (make-instance 'verilog-net :name "addr")))
                             (outputs (list (make-instance 'verilog-net :name "ready")
                                            (make-instance 'verilog-net :name "data"))))

                         (generate-verilog
                          (make-instance 'verilog-module
                                         :name (chil-sym->verilog-sym "test-parameters")
                                         :parameters '(("EMPTY") ("IN_WIDTH" 32) ("out-width" 16))
                                         :inputs inputs
                                         :outputs outputs)))))

(define-test generate-verilog-invalid-parameters ()
  (assert-error 'simple-error
                (let ((inputs (list (make-instance 'verilog-net :name "valid")
                                    (make-instance 'verilog-net :name "addr")))
                      (outputs (list (make-instance 'verilog-net :name "ready")
                                     (make-instance 'verilog-net :name "data"))))
                  (generate-verilog
                   (make-instance 'verilog-module
                                  :name (chil-sym->verilog-sym "invalid-parameters")
                                  :parameters '(("IN_WIDTH" 32) ("failing" 16 2))
                                  :inputs inputs
                                  :outputs outputs)))))

(define-test generate-verilog-assign ()
  (assert-string-equal
   "assign DEV = foo;"
   (generate-verilog
    (make-instance 'verilog-assign
                   :target (make-instance 'verilog-net :name "DEV")
                   :body (make-instance 'verilog-net :name "foo")))))

(defparameter *test-module-body*
  (let ((inputs (list (make-instance 'verilog-net :name "a")))
        (outputs (list (make-instance 'verilog-net :name "F"))))
    (make-instance 'verilog-module
                   :name (chil-sym->verilog-sym "test-body")
                   :inputs inputs
                   :outputs outputs
                   :body (make-instance 'verilog-assign :target (first outputs)
                                                        :body (first inputs))))
  "CHIL Module for testing that is a complete module, but unparameterized.")

(define-test generate-verilog-body ()
  (assert-string-equal "module test_body (
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
