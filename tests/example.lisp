(defpackage :chil/tests
  (:use :cl :lisp-unit2 :check-it)
  (:shadowing-import-from #:lisp-unit2 #:assert-error))

(in-package :chil/tests)

(define-test example ()
  (assert-eql 1 (- 2 1)))

(defmacro chil/tests::assert-error (condition form)
  "Return 't if FORM raises CONDITION, else return NIL."
  `(uiop:emptyp
    (multiple-value-list
     (lisp-unit2:assert-error ,condition ,form))))

(lisp-unit2:define-test log2up-random ()
  ;; This test SHOULD throw an error
  (check-it:check-it
   (check-it:generator (integer * -1))
   ;; (lambda (x)
   ;;   (uiop:emptyp
   ;;    (multiple-value-list
   ;;     (lisp-unit2:assert-error 'simple-error (chil/utils:log2up x)))))))
   (lambda (x)
     (chil/tests::assert-error 'simple-error (chil/utils:log2up x)))))

;; The test above fails because lisp-unit2:assert-error does not return a value
;; when the form being evaluated DOES raise the specified condition/error!
;; See:
;; CL-USER> (lisp-unit2:assert-error 'simple-error (chil/utils:log2up -1))
;; ; No values
;; check-it relies on the lambda form returning a boolean to know whether or not
;; the property is actually correct.
;; Parachute may make this easier, I am not sure. I would need to tinker with
;; parachute:fail inside the lambda some more.

;; The test below will always be correct, because the guard prevents odd numbers
;; from being generated as an input to the lambda.
(define-test check-it-example ()
  (assert-true
   (check-it:check-it (check-it:generator (check-it:guard #'evenp (integer)))
                      (lambda (x) (evenp x)))))

;; The test below will not always be correct, because there are generated cases
;; where the input will produce an incorrect output.
;; check-it:check-it returns 'nil (which is false-y) when there is even ONE test
;; case that is incorrect. assert-false means we catch that as an "expected"
;; error.
;; (define-test check-it-example-failing ()
;;   (assert-false
;;    (check-it:check-it (check-it:generator (integer))
;;                       (lambda (x) (evenp x)))))
