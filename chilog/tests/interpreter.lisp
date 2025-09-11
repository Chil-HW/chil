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

(define-test unify ()
  ;; Unify a variable with another fact.
  ;; This is an aliasing operation.
  ;; test("foo") :- .
  ;; unify-test(X) :- test(X).
  (let* ((subs (make-hash-table :test #'equal))
         (X (make-instance 'chilog-variable :name "X"))
         (unify-possible? (chilog/interpreter:unify
                           (make-instance
                            'chilog-atom
                            :predicate "test"
                            :terms (list X))
                           (list "foo")
                           subs)))
    (assert-true unify-possible?)
    (assert-equalp
     (chilog/interpreter:alist->substitution
      `((,X . "foo")))
     subs))

  ;; test("foo", "bar") :- .
  ;; unify-test(X, Y) :- test(X, Y).
  (let* ((sub (make-hash-table :test #'equal))
         (X (make-instance 'chilog-variable :name "X"))
         (Y (make-instance 'chilog-variable :name "Y"))
         (unify-possible? (chilog/interpreter:unify
                           (make-instance
                            'chilog-atom
                            :predicate "test"
                            :terms (list X Y))
                           (list "foo" "bar")
                           sub)))
    (assert-true unify-possible?)
    (assert-equalp
     (chilog/interpreter:alist->substitution
      `((,X . "foo") (,Y . "bar")))
     sub))

  ;; test("foo", "bar") :- .
  ;; unify-test(X, "foo") :- test(X, "foo")
  (let* ((sub (make-hash-table :test #'equal))
         (X (make-instance 'chilog-variable :name "X"))
         (unify-possible? (chilog/interpreter:unify
                           (make-instance
                            'chilog-atom
                            :predicate "test"
                            :terms (list X "foo"))
                           (list "foo" "bar")
                           sub)))
    (assert-false unify-possible?))

  ;; Test that unify returns false when we try to assign two different values to
  ;; the same variable.
  (let* ((sub (make-hash-table :test #'equal))
         (X (make-instance 'chilog-variable :name "X"))
         (unify-possible? 'nil))
    (setf (gethash X sub) "earlier")
    ;; If we attempt to unify X <- "bar", we must observe that X was previously
    ;; unified to have the value "earlier". So unification should fail!
    (setf unify-possible?
          (chilog/interpreter:unify
           (make-instance
            'chilog-atom
            :predicate "test"
            :terms (list X "foo"))
           (list "bar" "foo")
           sub))
    (assert-false unify-possible?)))

(define-test search-db ()
  ;; Searching the database for facts should provide us with all facts that we
  ;; installed.
  (let* ((dl (make-instance 'chilog-db))
         (parent (make-instance 'chilog-predicate
                                :name "parent"
                                :arity 2))
         (X (make-instance 'chilog-variable :name "X"))
         (Y (make-instance 'chilog-variable :name "Y"))
         (subs))
    (add-predicate! parent dl)
    (add-variable! X dl)
    (add-variable! Y dl)
    (add-fact! (list "alice" "bob") parent)
    (add-fact! (list "bob" "carol") parent)
    (setf subs
          (chilog/interpreter::search-db
           dl
           0
           (list (predicate->atom parent (list X Y)))
           (make-hash-table :test #'equal)))
    (assert-set-equal
     (chilog/interpreter:alist->substitutions
      `((,X . "alice") (,Y . "bob"))
      `((,X . "bob") (,Y . "carol")))
     subs
     :test #'equalp)))

(define-test evaluate ()
  ;; Evaluating a predicate with only facts should work. This occurs in the case
  ;; where we are evaluating an "aliasing" predicate.
  ;; We expect this to yield the installed facts
  (let* ((dl (make-instance 'chilog-db))
         (parent (make-instance 'chilog-predicate
                                :name "parent"
                                :arity 2))
         (ancestor (make-instance 'chilog-predicate
                                  :name "ancestor"
                                  :arity 2))
         (X (make-instance 'chilog-variable :name "X"))
         (Y (make-instance 'chilog-variable :name "Y"))
         (subs))
    (add-predicate! parent dl)
    (add-variable! X dl)
    (add-variable! Y dl)
    (add-fact! (list "alice" "bob") parent)
    (add-fact! (list "bob" "carol") parent)
    ;; ancestor(X, Y) :- parent(X, Y)
    ;; NOTE: This rule is unused right now.
    (add-rules! ancestor (list X Y) (predicate->atom parent (list X Y)))
    (setf subs (chilog/interpreter::evaluate
                dl
                (list (predicate->atom parent (list X Y)))))
    (assert-set-equal
     (chilog/interpreter:alist->substitutions
      `((,X . "alice") (,Y . "bob"))
      `((,X . "bob") (,Y . "carol")))
     subs
     :test #'equalp)))
