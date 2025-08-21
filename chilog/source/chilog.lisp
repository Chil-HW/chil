(defpackage :chilog
  (:use :cl)
  (:export #:hello))

(in-package :chilog)

(defun hello ()
  (format t "Hello to Chilog, Chil's Datalog implementation!~%"))
