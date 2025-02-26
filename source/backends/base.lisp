(defpackage :chil/backends
  (:use :cl :cl-ppcre)
  (:export #:generate
           #:verilog-generator
           #:vhdl-generator))

(in-package :chil/backends)

;;; Abstract HDL backend class
;; TODO: Make this a metaclass with MOP rather than just an "abstract base
;; class"?
(defclass backend-hdl ()
  ()
  (:documentation "Generic HDL back-end class. Do NOT initialize this class!

This is intended to be a way to retarget `Chil' modules to a particular
target."))

;; NOTE: To generate a backend language to a file, use the following invocation:
;; (uiop:with-output-file (file "file.ext" :if-exists :supersede)
;;  (generate (make-instance 'chil/backends:generator) module file))
(defgeneric codegen (backend-module stream)
  (:documentation "Abstract method to perform code generation on the provided
backend MODULE to STREAM. Every backend object that the user cares about should
provide an implementation for `codegen'."))
