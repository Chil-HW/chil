(defpackage :chil/tests/vhdl
  (:use :cl :lisp-unit2 :check-it
        :chil/backends :chil/backends/vhdl
        :chil/tests))

(in-package :chil/tests/vhdl)

(defun generate-vhdl (vmodule)
  "Helper function to generate VHDL modules. Returns the generated VHDL as
a string."
  (uiop:with-output (str 'nil)
    (chil/backends:codegen vmodule str)))

(defparameter *test-module-empty*
  (make-instance 'vhdl-module :name "test-empty")
  "CHIL Module for testing that has no inputs, no outputs, and no body.")

(defparameter *test-module-inputs*
  (let ((inputs (list (make-instance 'vhdl-net :name "valid")
                      (make-instance 'vhdl-net :name "addr"))))
    (make-instance 'vhdl-module :name "test-inputs"
                                :inputs inputs))
  "CHIL Module for testing that has only inputs, no outputs, and no body.")

(defparameter *test-module-outputs*
  (let ((outputs (list (make-instance 'vhdl-net :name "ready")
                       (make-instance 'vhdl-net :name "data"))))
    (make-instance 'vhdl-module :name "test-outputs"
                                :outputs outputs))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(defparameter *test-module-inputs-outputs*
  (let ((inputs (list (make-instance 'vhdl-net :name "valid")
                      (make-instance 'vhdl-net :name "addr")))
        (outputs (list (make-instance 'vhdl-net :name "ready")
                       (make-instance 'vhdl-net :name "data"))))
    (make-instance 'vhdl-module :name "test-inputs-outputs"
                                :inputs inputs
                                :outputs outputs))
  "CHIL Module for testing that has only outputs, no inputs, and no body.")

(define-test generate-vhdl-empty-module ()
  (assert-string-equal "entity test_empty is
port ();
end entity test_empty;

architecture rtl of test_empty is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-empty*)))

(define-test generate-vhdl-module-only-inputs ()
  (assert-string-equal "entity test_inputs is
port (valid : in std_logic;
addr : in std_logic);
end entity test_inputs;

architecture rtl of test_inputs is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-inputs*)))

(define-test generate-vhdl-module-only-outputs ()
  (assert-string-equal "entity test_outputs is
port (ready : out std_logic;
data : out std_logic);
end entity test_outputs;

architecture rtl of test_outputs is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-outputs*)))

(define-test generate-vhdl-module-inputs-outputs ()
  (assert-string-equal "entity test_inputs_outputs is
port (valid : in std_logic;
addr : in std_logic;
ready : out std_logic;
data : out std_logic);
end entity test_inputs_outputs;

architecture rtl of test_inputs_outputs is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-inputs-outputs*)))
