(defpackage :chilog/tests
  (:use :cl :lisp-unit2)
  ;; NOTE: Do NOT add :chilog to the :use list! It interferes with defining
  ;; tests, preventing the tests from running.
  (:export #:assert-set-equal
           #:assert-set-not-equal))

(in-package :chilog/tests)

;;; These tests frequently use eval to prevent Common Lisp compilers from
;;; attempting to create objects ahead-of-time or doing slot existence/type
;;; checking at compile-time, since these tests can first be compiled and THEN
;;; run.
;;; If we did NOT use eval, then a compiler can detect that slots are missing or
;;; incorrectly typed and refuse the test, even though we deliberately WANT to
;;; test if the RUNTIME will throw the errors that we want, not the compiler.

(defmacro assert-set-equal (&whole whole expected form
                            &rest extras
                            &key (test #'equal))
  "Assert whether expected and form are equal as sets, using the equality test
provided in :TEST. By default :TEST is set to `equal'."
  ;; XXX: lisp-unit2 does not export these 2 symbols, so we have rely on them
  ;; having this name!
  `(lisp-unit2::expand-assert
    'lisp-unit2::equal-result ,form ,form ,expected ,extras
    :test #'(lambda (s1 s2) (not (set-exclusive-or s1 s2 :test ,test)))
    :full-form ',whole))

(defmacro assert-set-not-equal (&whole whole expected form
                                &rest extras
                                &key (test #'equal))
  "Assert whether expected and form are not equal as sets, using the equality
test provided in :TEST. By default :TEST is set to `equal'."
  ;; XXX: lisp-unit2 does not export these 2 symbols, so we have rely on them
  ;; having this name!
  `(lisp-unit2::expand-assert
    'lisp-unit2::equal-result ,form ,form ,expected ,extras
    :test #'(lambda (s1 s2) (set-exclusive-or s1 s2 :test ,test))
    :full-form ',whole))

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
