(defpackage :chilog/tests
  (:use :cl :lisp-unit2))

(in-package :chilog/tests)

(define-test example ()
  (assert-eql 1 (- 2 1)))
