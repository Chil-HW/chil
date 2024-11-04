(in-package :chil/backends)

;;; VHDL Generator
(defclass vhdl-generator (hdl-generator)
  (:documentation "An HDL back-end for generating VHDL."))

(defmethod generate ((generator vhdl-generator) module stream)
  "Generate VHDL for the provided MODULE."
  ())
