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

(defun generate-vhdl (module)
  "Helper function to generate VHDL modules. Returns the generated Vhdl as
a string."
  (uiop:with-output (str 'nil)
    (chil/backends:generate
     (make-instance 'chil/backends:vhdl-generator) module str)))

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
port (addr : in std_logic;
ready : in std_logic);
end entity test_inputs;

architecture rtl of test_inputs is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-inputs*)))

(define-test generate-vhdl-module-only-outputs ()
  (assert-string-equal "entity test_outputs is
port (addr : out std_logic;
valid : out std_logic);
end entity test_outputs;

architecture rtl of test_outputs is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-outputs*)))

(define-test generate-vhdl-module-inputs-outputs ()
  (assert-string-equal "entity test_inputs_outputs is
port (addr : in std_logic;
valid : in std_logic;
addr : out std_logic;
valid : out std_logic);
end entity test_inputs_outputs;

architecture rtl of test_inputs_outputs is
begin
body;
end architecture rtl;"
                (generate-vhdl *test-module-inputs-outputs*)))
