(in-package :chil/backends)

;;; SystemVerilog Generator
(defclass systemverilog-generator (hdl-generator)
  ()
  (:documentation "An HDL back-end for generating SystemVerilog."))

(defmethod generate ((generator systemverilog-generator) module stream)
  "Generate SystemVerilog for the provided MODULE.")
