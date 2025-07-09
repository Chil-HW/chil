(defsystem :chil
  :author "Karl Hallsby <karl@hallsby.com>"
  :description "Constructing Hardware In Lisp"
  :pathname #p"source/"
  :components ((:file "chil")
               (:module "Module"
                :pathname ""
                :depends-on ("chil")
                :components ((:file "module")))
               (:module "IR"
                :pathname #p""
                :components ((:file "ir")))
               (:module "Backends"
                :pathname #p"backends/"
                :depends-on ("Module")
                :components ((:file "base")
                             (:module "Verilog"
                              ;; Verilog "module" not under subdir right now.
                              :pathname #p""
                              :components ((:file "verilog")))
                             (:module "VHDL"
                              :pathname #p""
                              :components ((:file "vhdl")))
                             (:file "system-verilog")))
               (:module "Utils"
                :pathname ""
                :depends-on ()
                :components ((:file "utils")))
               (:module "Types"
                :pathname "types/"
                :depends-on ("Utils")
                :components ((:file "data")
                             (:file "uint"))))
  :depends-on (:log4cl :cl-ppcre :trivia)
  :in-order-to ((test-op (test-op "chil/tests"))))

(defun program-installed-p (prog-name)
  "Determine if the program with binary PROG-NAME is installed and available to
the Lisp image. PROG-NAME is expected to be a string that is present in `$PATH'.

Return `nil' if the program is NOT installed, `t' otherwise."
  (multiple-value-bind (stdout stderr rc)
      (uiop:run-program prog-name
                        ;; Ignore cases where prog-name is
                        ;; installed but we called it wrong.
                        :ignore-error-status 't)
    (declare (ignore stdout stderr))
    ;; 127 is the default return code when a program is not locatable in $PATH.
    (not (= rc 127))))

(defvar verilator-installed-p
  (program-installed-p "verilator")
  "Is Verilator installed?

This is used to control whether or not simulation-based unit tests should be
run.")

(defsystem :chil/tests
  :depends-on (:chil :alexandria :lisp-unit2 :check-it)
  :pathname #p"tests/"
  :components ((:file "base")
               ;; TODO: Convert base & utils to a single module
               (:file "utils")
               (:module "Types"
                :pathname #p"types/"
                :depends-on ("utils")
                :components ((:file "uint")))
               (:module "Verilog-tests"
                :pathname #p""
                :depends-on ("base" "utils")
                :components ((:file "write-verilog")
                             (:file "sim")))
               (:module "VHDL-tests"
                :pathname #p""
                :depends-on ("base" "utils")
                :components ((:file "write-vhdl")))))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :chil/tests))))
  (let ((packages (list (find-package :chil/tests)
                        (find-package :chil/tests/verilog)
                        (find-package :chil/tests/vhdl))))
    (uiop:symbol-call
     :lisp-unit2 :run-tests
     :package packages
     :name :chil
     :exclude-tags (append
                    (unless verilator-installed-p '(:verilator))
                    (unless (program-installed-p "ghdl") '(:ghdl)))
     :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))

(defsystem :chil/sim
  :author "Karl Hallsby <karl@hallsby.com>"
  :description "Chil Snapshotting Digital Simulator"
  :pathname #p"source/sim/"
  :components ((:file "sim"))
  :depends-on (:log4cl))
