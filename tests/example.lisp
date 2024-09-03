(defpackage :chil/tests
  (:use :cl :parachute :check-it))

(in-package :chil/tests)

(define-test example ()
  (parachute:is eql
    1
    (- 2 1)))

;; The test below will always be correct, because the guard prevents odd numbers
;; from being generated as an input to the lambda.
(define-test check-it-example
  (parachute:true
   (check-it:check-it (check-it:generator (check-it:guard #'evenp (integer)))
    (lambda (x) (evenp x)))))

;; The test below will not always be correct, because there are generated cases
;; where the input will produce an incorrect output.
;; check-it:check-it returns 'nil (which is false-y) when there is even ONE test
;; case that is incorrect. assert-false means we catch that as an "expected"
;; error.
;; (define-test check-it-example-failing
;;   (check-it:check-it (check-it:generator (integer))
;;                      (lambda (x) (parachute:false (evenp x)))))

;; When check-it performs a randomized test, the test returns 't if ALL
;; randomly-generated test cases were successful, and 'nil otherwise. An
;; individual test case is successful when it returns 't, and a failure
;; otherwise. The lambda form is supposed to satisfy that output format.
