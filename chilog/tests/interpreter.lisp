(defpackage :chilog/tests/interpreter
  (:use :cl :lisp-unit2
        :chilog :chilog/tests/utils))

(in-package :chilog/tests/interpreter)

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
  (let ((sub (make-hash-table :test #'equal))
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
  (let ((dl (make-instance 'chilog-db))
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
  (let ((dl (make-instance 'chilog-db))
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

(define-test infer ()
  ;; Performing inference on an "aliasing" predicate should propagate the facts
  ;; up from the underlying "pointed-to" predicate to the "pointed-by" one. The
  ;; end result we expect is that we yield the same thing as the original
  ;; predicate.
  (let ((dl (make-instance 'chilog-db))
        (inner (make-instance 'chilog-predicate
                              :name "inner"
                              :arity 1))
        (outer (make-instance 'chilog-predicate
                              :name "outer"
                              :arity 1))
        (X (make-instance 'chilog-variable :name "X")))
    (add-predicate! inner dl)
    (add-predicate! outer dl)
    (add-variable! X dl)
    (add-fact! (list "foo") inner)
    ;; outer(X) :- inner(X)
    ;; Install an aliasing rule
    (add-rules! outer (list X) (predicate->atom inner (list X)))
    (chilog/interpreter:infer dl)
    (assert-set-equal
     '(("foo"))
     (facts (gethash "outer" (chilog:table->hash-table (predicates dl))))
     :test #'equal))

  (let ((dl (make-instance 'chilog-db))
        (parent (make-instance 'chilog-predicate
                               :name "parent"
                               :arity 2))
        (ancestor (make-instance 'chilog-predicate
                                 :name "ancestor"
                                 :arity 2))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y")))
    (add-predicate! parent dl)
    (add-predicate! ancestor dl)
    (add-variable! X dl)
    (add-variable! Y dl)
    (add-fact! (list "alice" "bob") parent)
    (add-fact! (list "bob" "carol") parent)
    ;; ancestor(X, Y) :- parent(X, Y)
    (add-rules! ancestor (list X Y) (predicate->atom parent (list X Y)))
    ;; Perform inference, so the facts installed for parent/2 should propagate
    ;; up to ancestor/2
    (chilog/interpreter:infer dl)
    (assert-set-equal
     '(("alice" "bob")
       ("bob" "carol"))
     (facts (gethash "ancestor" (chilog:table->hash-table (predicates dl))))
     :test #'equal))

  ;; Perform inference on a Datalog program with a recursive rule that should
  ;; "learn" something.
  (let ((dl (make-instance 'chilog-db))
        (parent (make-instance 'chilog-predicate
                               :name "parent"
                               :arity 2))
        (ancestor (make-instance 'chilog-predicate
                                 :name "ancestor"
                                 :arity 2))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z")))
    (add-variable! X dl)
    (add-variable! Y dl)
    (add-variable! Z dl)
    (add-predicate! parent dl)
    (add-predicate! ancestor dl)
    (add-fact! (list "alice" "bob") parent)
    (add-fact! (list "bob" "carol") parent)
    (add-rules! ancestor (list X Y) (predicate->atom parent (list X Y)))
    ;; The recursive rule for ancestor/2
    (add-rules! ancestor (list X Y)
                (predicate->atom parent (list X Z))
                (predicate->atom ancestor (list Z Y)))
    (chilog/interpreter:infer dl)
    ;; Now verify that the ancestor/2 predicate itself carries the facts
    (assert-set-equal
     '(("alice" "carol") ("bob" "carol") ("alice" "bob"))
     (facts (gethash "ancestor" (chilog:table->hash-table (predicates dl))))
     :test #'equal)
    (assert-set-equal
     '(("alice" "carol") ("bob" "carol") ("alice" "bob"))
     (facts ancestor)
     :test #'equal))
  )

(define-test query ()
  ;; A query with a variable and a constant that matches a fact we installed
  ;; should return the fact.
  (let ((dl (make-instance 'chilog-db))
        (parent (make-instance 'chilog-predicate
                               :name "parent"
                               :arity 2))
        (ancestor (make-instance 'chilog-predicate
                                 :name "ancestor"
                                 :arity 2))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y")))
    (add-variable! X dl)
    (add-variable! Y dl)
    (add-predicate! parent dl)
    (add-predicate! ancestor dl)
    (add-fact! (list "alice" "bob") parent)
    (add-fact! (list "bob" "carol") parent)
    (add-rules! ancestor (list X Y) (predicate->atom parent (list X Y)))
    (chilog/interpreter:infer dl)
    ;; We expect this query to return "bob", since that is the fact we provided.
    (assert-set-equal
     (chilog/interpreter:alist->substitutions
      `((,X . "bob")))
     (chilog/interpreter:query dl (predicate->atom parent (list X "carol")))
     :test #'equalp)
    ;; Since ancestor is just an alias for parent, the same query against the
    ;; ancestor predicate should return the same thing.
    (assert-set-equal
     (chilog/interpreter:alist->substitutions
      `((,X . "bob")))
     (chilog/interpreter:query dl (predicate->atom ancestor (list X "carol")))
     :test #'equalp))

  ;; Run a query in a Datalog program that has a recursive rule that should
  ;; "learn" something.
  (let ((dl (make-instance 'chilog-db))
        (parent (make-instance 'chilog-predicate
                               :name "parent"
                               :arity 2))
        (ancestor (make-instance 'chilog-predicate
                                 :name "ancestor"
                                 :arity 2))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z")))
    (add-variable! X dl)
    (add-variable! Y dl)
    (add-variable! Z dl)
    (add-predicate! parent dl)
    (add-predicate! ancestor dl)
    (add-fact! (list "alice" "bob") parent)
    (add-fact! (list "bob" "carol") parent)
    (add-rules! ancestor (list X Y) (predicate->atom parent (list X Y)))
    ;; The recursive rule for ancestor/2
    (add-rules! ancestor (list X Y)
                (predicate->atom parent (list X Z))
                (predicate->atom ancestor (list Z Y)))
    (chilog/interpreter:infer dl)
    ;; Now that we have a recursive rule for ancestor/2, we should actually
    ;; "learn" information from the facts provided. In this case, BOTH alice
    ;; and bob are ancestors of carol.
    (assert-set-equal
     (chilog/interpreter:alist->substitutions
      `((,X . "alice")) ; Learned alice is ancestor of carol
      `((,X . "bob")))
     (chilog/interpreter:query dl (predicate->atom ancestor (list X "carol")))
     :test #'equalp)
    ;; Now verify that the ancestor/2 predicate itself carries these facts
    (assert-set-equal
     '(("alice" "carol") ("bob" "carol") ("alice" "bob"))
     (facts (gethash "ancestor" (chilog:table->hash-table (predicates dl))))
     :test #'equal)
    (assert-set-equal
     '(("alice" "carol") ("bob" "carol") ("alice" "bob"))
     (facts ancestor)
     :test #'equal))
  )
