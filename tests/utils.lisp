(in-package :chil/tests)

(define-test log2up-of-negative
  ;; This test SHOULD throw an error
  (parachute:true
   (check-it:check-it (check-it:generator (integer * -1))
    (lambda (x)
      (handler-case (chil/utils:log2up x)
        (simple-error (exp)
          (declare (ignore exp))
          't)
        ;; We are testing that negatives are caught, not that the function
        ;; actually produces the correct answer.
        (:no-error (exp) (declare (ignore exp)) nil))))))

(define-test log2up-of-zero
  (parachute:is #'= 1 (chil/utils:log2up 0)))

(define-test log2up-normal
  (parachute:is #'= 1 (chil/utils:log2up 1))
  (parachute:is #'= 2 (chil/utils:log2up 2))
  (parachute:is #'= 7 (chil/utils:log2up 127))
  (parachute:is #'= 8 (chil/utils:log2up 128))
  (parachute:is #'= 8 (chil/utils:log2up 129)))

(define-test log2up-property
  :depends-on (:and log2up-of-zero log2up-normal)
  (parachute:true
   (check-it:check-it
    (check-it:generator (check-it:guard #'plusp (integer)))
    (lambda (x)
      (= (max 1 (integer-length x))
         (chil/utils:log2up x))))))

(define-test log2up-random
  :depends-on (:and log2up-of-negative log2up-property)
  (parachute:true
   (check-it:check-it (check-it:generator (integer))
    (lambda (x)
      (handler-case (chil/utils:log2up x)
        (simple-error (exp)
          (declare (ignore exp))
          't)
        (:no-error (exp)
          (= (max 1 (integer-length x)) exp)))))))
