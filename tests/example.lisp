(defpackage :chil/tests
  (:use :cl :parachute :check-it))

(in-package :chil/tests)

(define-test example ()
  (parachute:is eql
    1
    (- 2 1)))

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
(define-test check-it-example-failing ()
  (assert-false
   (check-it:check-it (check-it:generator (integer))
                      (lambda (x) (evenp x)))))
