(defpackage :chilog/tests
  ;; NOTE: Do NOT add :chilog to the :use list! It interferes with defining
  ;; tests, preventing the tests from running.
  (:use :cl :lisp-unit2))

(in-package :chilog/tests)

;;; These tests frequently use eval to prevent Common Lisp compilers from
;;; attempting to create objects ahead-of-time or doing slot existence/type
;;; checking at compile-time, since these tests can first be compiled and THEN
;;; run.
;;; If we did NOT use eval, then a compiler can detect that slots are missing or
;;; incorrectly typed and refuse the test, even though we deliberately WANT to
;;; test if the RUNTIME will throw the errors that we want, not the compiler.

(define-test chilog-variable ()
  ;; Chilog variables MUST be provided names. We do NOT auto-generate names
  ;; currently.
  (assert-error
   'error
   (eval (make-instance 'chilog:chilog-variable)))

  ;; Creating a variable should be fine.
  (assert-true
   (eval (make-instance 'chilog:chilog-variable :name "FOO")))

  ;; Check the type and the predicate of the newly-created Chilog variable.
  (let ((FOO (make-instance 'chilog:chilog-variable :name "FOO")))
    (assert-typep
     'chilog:chilog-variable
     FOO)
    (assert-true (chilog:chilog-variable-p FOO)))
  )

(define-test chilog-value ()
  ;; Booleans
  (assert-true (chilog:chilog-value-p 't))
  (assert-true (chilog:chilog-value-p 'nil))

  ;; Numbers
  (assert-true (chilog:chilog-value-p -32))
  (assert-true (chilog:chilog-value-p 0))
  (assert-true (chilog:chilog-value-p 32))
  (assert-true (chilog:chilog-value-p 3.14))
  (assert-true (chilog:chilog-value-p #C(3 9)))
  (assert-true
   (check-it:check-it
    (check-it:generator
     ;; TODO: Generate complex numbers
     (or (integer) (real)))
    (lambda (x) (chilog:chilog-value-p x))))

  ;; Strings
  (assert-true (chilog:chilog-value-p ""))
  (assert-true (chilog:chilog-value-p "X"))
  (assert-true (chilog:chilog-value-p "fOo"))
  (assert-true (chilog:chilog-value-p "zazagl0rp"))
  (assert-true
   (check-it:check-it (check-it:generator (string))
    (lambda (s) (chilog:chilog-value-p s))))
  )

(define-test chilog-atom ()
  ;; A chilog-atom MUST have ALL their slots' values up front.
  ;; A chilog-atom is treated as immutable.
  (assert-error
   'error
   (eval (make-instance 'chilog:chilog-atom)))
  (assert-error
   'error
   (eval (make-instance 'chilog:chilog-atom
                        :predicate "missing-terms")))
  (assert-error
   'error
   (eval
    (let ((X (make-instance 'chilog:chilog-variable :name "X")))
      (make-instance 'chilog:chilog-atom
                     :terms (list 32 "hello" X)))))

  ;; Creating an atom with everything provided should work.
  (assert-true
   (let ((X (make-instance 'chilog:chilog-variable :name "X")))
     (eval (make-instance 'chilog:chilog-atom
                          :predicate "functional"
                          :terms (list 32 "hello" X)))))

  ;; chilog-atom should satisfy its own predicate.
  (assert-true
   (eval
    (let ((X (make-instance 'chilog:chilog-variable :name "X")))
      (chilog:chilog-atom-p
       (make-instance 'chilog:chilog-atom
                      :predicate "functional"
                      :terms (list 32 "hello" X))))))

  ;; list-of-chilog-atoms
  (assert-true
   (eval
    (let ((X (make-instance 'chilog:chilog-variable :name "X")))
      (chilog:list-of-chilog-atoms-p
       (list (make-instance 'chilog:chilog-atom
                            :predicate "only-vals"
                            :terms (list 32 "hello"))
             (make-instance 'chilog:chilog-atom
                            :predicate "with-var"
                            :terms (list X)))))))
  )
