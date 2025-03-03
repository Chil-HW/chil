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

(defsystem :chil/tests
  :depends-on (:chil :alexandria :lisp-unit2 :check-it)
  :pathname #p"tests/"
  :components ((:file "base")
               (:module "Verilog-tests"
                :pathname #p""
                :depends-on ("base")
                :components ((:file "write-verilog")))
               (:module "VHDL-tests"
                :pathname #p""
                :depends-on ("base")
                :components ((:file "write-vhdl")))
               (:file "utils")))

(defmethod asdf:perform ((o asdf:test-op) (c (eql (find-system :chil/tests))))
  (let ((packages (list (find-package :chil/tests)
                        (find-package :chil/tests/verilog)
                        (find-package :chil/tests/vhdl))))
    (uiop:symbol-call
     :lisp-unit2 :run-tests
     :package packages
     :name :chil
     :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))

(defsystem :chil/sim
  :author "Karl Hallsby <karl@hallsby.com>"
  :description "Chil Snapshotting Digital Simulator"
  :pathname #p"source/sim/"
  :components ((:file "sim"))
  :depends-on (:log4cl))
