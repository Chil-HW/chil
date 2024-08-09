(defpackage :chil/backends
  (:use :cl :cl-ppcre)
  (:export #:generate
           #:verilog-generator))

(in-package :chil/backends)

;;; Abstract HDL generator class
;; TODO: Make this a metaclass with MOP?
(defclass hdl-generator ()
  ())

(defgeneric generate (generator module stream)
  (:documentation "Abstract method to generate the provided CHIL MODULE as another HDL."))
