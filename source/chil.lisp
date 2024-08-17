(defpackage :chil
  (:use :cl)
  (:export #:hello
           #:time-spec
           #:time-spec-amount
           #:time-spec-units
           #:make-time-spec
           #:timescale
           #:module
           #:module-name
           #:module-parameters
           #:module-inputs
           #:module-outputs
           #:data
           #:pack
           #:unpack
           #:uint))

(in-package :chil)

(defun hello ()
  (format t "Hello to Chil!~&"))
