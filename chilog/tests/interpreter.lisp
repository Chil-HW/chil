(defpackage :chilog/tests/interpreter
  (:use :cl :lisp-unit2
        :chilog))

(in-package :chilog/tests/interpreter)

;; Given this rule:
;;  ancestor[X, Y] = parent[X, Z], ancestor[Z, Y]
;; With the following known facts:
;;  parent['bob', 'carol']
;;  parent['alice', 'bob']

;; We expect the following steps:
;; We start with the first fact. We unify X=’bob’ and Z=’carol’. We then call search recursively, moving to the next atom, ancestor[Z, Y], with that substitution in hand.

;; From the other rule of ancestor:

;; ancestor[X, Y] = parent[X, Y]

;; We probably already derived that every parent is an ancestor, so the current facts of ancestor are also:

;; ancestor['bob', 'carol']
;; ancestor['alice', 'bob']

;; We try to unify with the first fact and fail, because the substitution enforces Z=’carol’ which cannot unify with ‘bob’. The second fact also fails to unify, so this is a dead end.

;; That branch of the recursion dies and we’re back trying to unify the first atom. Now we unify with the second fact, obtaining the substitution X=’alice’ and Z=’bob’. We recurse again with that substitution.

;; We try to unify ancestor[Z, Y] with its first fact, and we succeed, because we can set Z=’bob’ and Y=’carol’. We managed to do a complete substitution, so we yield it.

;; TODO: Define tests for each piece in the interpreter

(define-test unify ()
  ;; Unifying an atom with a literal is always possible?
  (assert-true
   (chilog/interpreter:unify
    (make-instance
     'chilog-atom
     :predicate "test-atom"
     :terms '())
    (list 32)
    (make-hash-table))))
