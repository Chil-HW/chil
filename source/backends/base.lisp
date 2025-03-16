(defpackage :chil/backends
  (:use :cl)
  (:export #:backend-hdl
           #:codegen
           #:hyphen->underscore
           #:module-filename))

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

(defun hyphen->underscore (str)
  (let ((chars-to-replace (cl-ppcre:create-scanner "-")))
    (cl-ppcre:regex-replace-all chars-to-replace str "_")))

(defgeneric module-filename (backend-module)
  (:documentation "Return the filename representation this module should have
if/when this module is serialized to disk.

This will add the file extension and other information to the module's filename
that you would expect."))
