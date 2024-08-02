(defsystem :chil
  :author "Karl Hallsby <karl@hallsby.com>"
  :description "Constructing Hardware In Lisp"
  :pathname #p"source/"
  :components ((:file "chil")
               (:module "Module"
                :pathname ""
                :depends-on ("chil")
                :components ((:file "module")))
               (:module "Backends"
                :pathname #p"backends/"
                :depends-on ("Module")
                :components ((:file "base")
                             (:file "verilog")
                             (:file "vhdl")
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
  :depends-on (:log4cl)
  :in-order-to ((test-op (test-op "chil/tests"))))

(defsystem :chil/tests
  :depends-on (:chil :alexandria :parachute :check-it)
  :pathname #p"tests/"
  :perform (asdf:test-op (op c) (uiop:symbol-call :parachute :test :chil/tests))
  :components ((:file "example")
               (:file "write-verilog")
               (:file "utils")))

(defsystem :chil/sim
  :author "Karl Hallsby <karl@hallsby.com>"
  :description "Chil Snapshotting Digital Simulator"
  :pathname #p"source/sim/"
  :components ((:file "sim"))
  :depends-on (:log4cl))
