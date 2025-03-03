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
;;; Tests for various utilities used by the Verilog backend.
;;;

(define-test chil->verilog-symbol-conversion ()
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
  (assert-string-equal
   "module GENV_test_empty (
);
body;
endmodule // GENV_test_empty"
                (generate-verilog *test-module-empty*)))

(define-test generate-verilog-module-only-inputs ()
  (assert-string-equal
   "module GENV_test_inputs (
input GENV_valid,
input GENV_addr
);
body;
endmodule // GENV_test_inputs"
                (generate-verilog *test-module-inputs*)))

(define-test generate-verilog-module-only-outputs ()
  (assert-string-equal
   "module GENV_test_outputs (
output GENV_ready,
output GENV_data
);
body;
endmodule // GENV_test_outputs"
                (generate-verilog *test-module-outputs*)))

(define-test generate-verilog-module-inputs-outputs ()
  (assert-string-equal
   "module GENV_test_inputs_outputs (
input GENV_valid,
input GENV_addr,
output GENV_ready,
output GENV_data
);
body;
endmodule // GENV_test_inputs_outputs"
                (generate-verilog *test-module-inputs-outputs*)))

(define-test generate-verilog-module-parameters ()
  (assert-string-equal
   ;; FIXME: Parameters should not get the GENV prefix.
   "module GENV_test_parameters #(
parameter GENV_EMPTY,
parameter GENV_IN_WIDTH = 32,
parameter GENV_out_width = 16
)
(
input GENV_valid,
input GENV_addr,
output GENV_ready,
output GENV_data
);
body;
endmodule // GENV_test_parameters"
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
  (assert-string-equal
   "module GENV_test_body (
input GENV_a,
output GENV_F
);
assign F = a;
endmodule // GENV_test_body"
   (generate-verilog *test-module-body*)))

(define-test generate-verilog-assign ()
  (assert-string-equal
   "assign DEV = foo;"
   (generate-verilog
    (make-instance 'verilog-assign
                   :target (make-instance 'verilog-net :name "DEV")
                   :body (make-instance 'verilog-net :name "foo")))))

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
     'verilog-binop :op 'bit-xor
     :lhs (make-instance 'verilog-net :name "foo")
     :rhs (make-instance 'verilog-net :name "BAR")))))

(define-test generate-verilog-assign-binop ()
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

(define-test passthrough-module ()
  (assert-string-equal
   "module GENV_passthrough (
input GENV_x,
output GENV_f
);
assign f = x;
endmodule // GENV_passthrough"
   (let ((inputs (list (make-instance 'verilog-net :name "x")))
         (outputs (list (make-instance 'verilog-net :name "f"))))
     (generate-verilog
      (make-instance 'verilog-module
                     :name (chil-sym->verilog-sym "passthrough")
                     :inputs inputs
                     :outputs outputs
                     :body (make-instance 'verilog-assign
                                          :target (first outputs)
                                          :body (first inputs)))))))

(define-test combinatorial-binop-module ()
  (assert-string-equal
   "module GENV_comb_binop (
input GENV_x,
input GENV_y,
output GENV_f
);
assign f = x + y;
endmodule // GENV_comb_binop"
   (let* ((inputs (list (make-instance 'verilog-net :name "x")
                        (make-instance 'verilog-net :name "y")))
          (outputs (list (make-instance 'verilog-net :name "f")))
          (binop (make-instance 'verilog-binop :op '+
                                               :lhs (first inputs)
                                               :rhs (second inputs))))
     (generate-verilog
      (make-instance 'verilog-module
                     :name (chil-sym->verilog-sym "comb-binop")
                     :inputs inputs
                     :outputs outputs
                     :body (make-instance 'verilog-assign
                                          :target (first outputs)
                                          :body binop))))))
