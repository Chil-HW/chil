(defpackage :chil/utils
  (:use :cl)
  (:export #:log2up)
  (:documentation "General Utilities for working with Chil."))

(in-package :chil/utils)

(defun log2up (val)
  "Compute the number of bits required to represent VAL in binary."
  (assert (and (integerp val)
               (not (minusp val))))
  (max 1 ;; If val = 0, then the next expr |- 0, but we need 1 bit to represent 0
       (values (ceiling (log (1+ val) 2)))))
