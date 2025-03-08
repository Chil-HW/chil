(in-package :chil/tests)

(define-test log2up-of-zero ()
  (assert-eql 1 (chil/utils:log2up 0)))

(define-test log2up-of-negative ()
  ;; This test SHOULD throw an error
  (assert-error 'simple-error (chil/utils:log2up -1)))

(define-test log2up-normal ()
  (assert-equal 1 (chil/utils:log2up 1))
  (assert-equal 2 (chil/utils:log2up 2))
  (assert-equal 7 (chil/utils:log2up 127))
  (assert-equal 8 (chil/utils:log2up 128))
  (assert-equal 8 (chil/utils:log2up 129)))
