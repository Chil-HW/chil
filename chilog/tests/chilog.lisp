(defpackage :chilog/tests
  ;; NOTE: Do NOT add :chilog to the :use list! It interferes with defining
  ;; tests, preventing the tests from running.
  (:use :cl :lisp-unit2
        :chilog/tests/utils))

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

(define-test chilog-rule ()
  ;; A chilog-rule MUST have ALL their slots' values up front.
  ;; A chilog-rule is treated as immutable.
  (assert-error
   'error
   (eval (make-instance 'chilog:chilog-rule)))
  (assert-error
   'error
   (eval
    (make-instance 'chilog:chilog-rule
                   :head (make-instance
                          'chilog:chilog-atom
                          :predicate "missing-body"
                          :terms '()))))
  (assert-error
   'error
   (eval
    (make-instance 'chilog:chilog-rule
                   :body
                   (list
                    (make-instance 'chilog:chilog-atom
                                   :predicate "missing-head"
                                   :terms '())))))

                       ;; :terms (list 32 "hello" X)))))


  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (Y (make-instance 'chilog:chilog-variable :name "Y"))
         (rule-head (make-instance 'chilog:chilog-atom
                                   :predicate "head"
                                   :terms (list X Y)))
         (rule-body (list (make-instance 'chilog:chilog-atom
                                         :predicate "head"
                                         :terms (list Y X)))))
    ;; Creating a rule with everything provided should work.
    (assert-true
     (eval
      (make-instance 'chilog:chilog-rule
                     :head rule-head
                     :body rule-body)))
    ;; chilog-rule should satisfy its own predicate.
    (assert-true
     (eval
      (chilog:chilog-rule-p
       (make-instance 'chilog:chilog-rule
                      :head rule-head
                      :body rule-body)))))

  ;; list-of-chilog-rules
  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (Y (make-instance 'chilog:chilog-variable :name "Y"))
         (rule-body (list (make-instance 'chilog:chilog-atom
                                         :predicate "head"
                                         :terms (list Y X)))))
    (assert-true
     (eval
      (chilog:list-of-chilog-rules-p
       (list
        (make-instance 'chilog:chilog-rule
                       :head (make-instance 'chilog:chilog-atom
                                            :predicate "head0"
                                            :terms (list X Y))
                       :body rule-body)
        (make-instance 'chilog:chilog-rule
                       :head (make-instance 'chilog:chilog-atom
                                            :predicate "head1"
                                            :terms (list X Y))
                       :body rule-body))))))
  )

(define-test chilog-predicate ()
  ;; You MUST provide a name and arity to construct a predicate
  ;; Note that we do NOT require that all facts and/or rules be provided
  ;; up-front!
  (assert-error
   'error
   (eval (make-instance 'chilog:chilog-predicate)))
  (assert-error
   'error
   (eval
    (make-instance 'chilog:chilog-predicate
                   :name "missing-arity")))
  (assert-error
   'error
   (eval
    (make-instance 'chilog:chilog-predicate
                   :arity 0)))
  ;; A predicate's arity cannot be negative
  (assert-error
   'error
   (eval
    (make-instance 'chilog:chilog-predicate
                   :name "negative-arity"
                   :arity -3)))

  ;; The "setf-ers" for facts and rules just "blindly" accept anything and
  ;; everything provided to them, so long as the type of the provided values
  ;; lines up with the slots' declared :type.
  ;; setf facts
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 1))
        (new-facts '(("x")
                     (32))))
    (assert-number-equal 0
      (length (chilog:facts pred)))
    ;; A fact is an ordered list of values that each variable in the predicate's
    ;; arity is allowed to have.
    (setf (chilog:facts pred) new-facts)
    (assert-number-equal 2
      (length (chilog:facts pred)))
    (assert-set-equal new-facts
      (chilog:facts pred)
      :test #'equal))

  ;; setf rules
  (let* ((X (make-instance 'chilog:chilog-variable
                           :name "X"))
         (pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 1))
         (new-rules (list
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X)))))))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    ;; A rule is an unordered list of rules that is attached to the predicate.
    (setf (chilog:rules pred) new-rules)
    (assert-number-equal 1
      (length (chilog:rules pred)))
    (assert-equal new-rules
      (chilog:rules pred)
      :test #'equal))

  ;; The actual setters defined on the predicate class's modifiable slots add
  ;; the provided information to the predicate object while obeying the rules
  ;; Datalog/Chilog predicates must follow (rules & facts must be unique, being
  ;; the primary one).

  ;; add-fact!
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 1))
        ;; A fact is an ordered list of values that each variable in the
        ;; predicate's arity is allowed to have.
        (new-facts '(("x")
                     (32)
                     ("x"))))
    (assert-number-equal 0
      (length (chilog:facts pred)))
    (mapc (lambda (fact) (chilog:add-fact! fact pred))
          new-facts)
    (assert-number-equal 2
      (length (chilog:facts pred)))
    (assert-set-equal new-facts
      (chilog:facts pred)
      :test #'equal))

  ;; Adding a malformed fact should not work
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 1)))
    (assert-number-equal 0
      (length (chilog:facts pred)))
    (assert-error
     'type-error
     (chilog:add-fact! "not-a-fact" pred))
    (assert-number-equal 0
      (length (chilog:facts pred)))
    (assert-set-equal '()
      (chilog:facts pred)
      :test #'equal))

  ;; A predicate's facts are a set, so adding duplicates should do nothing
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 1))
        (duplicated-facts '(("duplicate")
                            ("duplicate"))))
    (assert-number-equal 0
      (length (chilog:facts pred)))
    (mapc (lambda (fact) (chilog:add-fact! fact pred))
          duplicated-facts)
    (assert-number-equal 1
      (length (chilog:facts pred)))
    (assert-set-equal (remove-duplicates duplicated-facts :test #'equal)
      (chilog:facts pred)
      :test #'equal))

  ;; add-rule!
  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (Y (make-instance 'chilog:chilog-variable :name "Y"))
         (pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 1))
         (new-rules (list
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X))))
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list Y))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list Y)))))))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    (mapc (lambda (rule) (chilog:add-rule! rule pred))
        new-rules)
    (assert-number-equal 2
      (length (chilog:rules pred)))
    (assert-set-equal new-rules
      (chilog:rules pred)
      :test #'equal))

  ;; Adding a malformed rule should not work
  (let ((X (make-instance 'chilog:chilog-variable :name "X"))
        (pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 1)))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    (assert-error
     'type-error
     (chilog:add-rule!
      ;; You cannot add a list of rules with add-rule!, only a single rule.
      (list
       (make-instance 'chilog:chilog-rule
                      :head (make-instance 'chilog:chilog-atom
                                           :predicate "test"
                                           :terms (list X))
                      :body (list
                             (make-instance 'chilog:chilog-atom
                                            :predicate "body"
                                            :terms (list X)))))
      pred))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    (assert-set-equal '()
      (chilog:rules pred)
      :test #'equal))

  ;; A predicate's rules are a set, so adding duplicates should do nothing
  ;; NOTE: We have two kinds of duplicates:
  ;;   1. Literally the same rule (add-rule! X pred) (add-rule! X pred)
  ;;   2. The same predicate, but different instances.
  ;; We currently only detect the first case.
  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 1))
         (new-rule (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X))))))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    ;; Adding a duplicate rule (according to #'eq) should only work once.
    (chilog:add-rule! new-rule pred)
    (chilog:add-rule! new-rule pred)
    (assert-number-equal 1
      (length (chilog:rules pred)))
    (assert-set-equal (list new-rule)
      (chilog:rules pred)
      :test #'equal))

  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 1))
         (new-rules (list
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X))))
                     ;; Two very similar but different rules
                     ;; They are not the same according to #'eq, but SHOULD be
                     ;; according to #'equal.
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X)))))))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    (mapc (lambda (rule) (chilog:add-rule! rule pred))
        new-rules)
    ;; FIXME: Fix this failing assertion.
    ;; (assert-number-equal 1
    ;;   (length (chilog:rules pred)))
    (assert-set-equal (remove-duplicates new-rules :test #'equal)
      (chilog:rules pred)
      :test #'equal))

  ;; TODO: add-rules! is hard to test since we do not have #'equal deifned for
  ;; predicates, atoms, etc.
  ;; add-rules!
  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (Y (make-instance 'chilog:chilog-variable :name "Y"))
         (pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 1))
         (new-rules (list
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X))))
                     (make-instance 'chilog:chilog-rule
                                    :head (make-instance 'chilog:chilog-atom
                                                         :predicate "test"
                                                         :terms (list Y))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list Y)))))))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    ;; (chilog:add-rules! pred (list X)
    ;;                    (nth 0 (chilog:body (nth 0 new-rules))))
    ;; (chilog:add-rules! pred (list X)
    ;;                    (make-instance 'chilog:chilog-atom
    ;;                                   :predicate "body"
    ;;                                   :terms (list X)))
    ;; (chilog:add-rules! pred (list Y)
    ;;                    (make-instance 'chilog:chilog-atom
    ;;                                   :predicate "body"
    ;;                                   :terms (list Y)))
    (assert-number-equal 2
      (length (chilog:rules pred)))
    (assert-set-equal new-rules
      (chilog:rules pred)
      :test #'equal)
    )

  ;; predicate->atom
  ;;  Will have to somehow define #'equal for my classes.
  )

(define-test chilog-db ()
  ;; Constructing a brand-new empty DB should always work.
  ;; There are no mandatory fields in the DB. It is intended to track all
  ;; variables and predicates in the program in a single structure. You CAN
  ;; provide these values at construction-time or by using the imperative
  ;; add-<thing>! methods.
  (assert-true
   (make-instance 'chilog:chilog-db))

  ;; Constructing a database with values provided should work.

  ;;; setf add-variable
  ;; You can set a whole table
  (let ((new-vars (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-variable-p))
        (db (make-instance 'chilog:chilog-db)))
    (assert-false (eq (chilog:variables db) new-vars))
    (setf (chilog:variables db) new-vars)
    (assert-true (eq (chilog:variables db) new-vars)))

  ;; You cannot set the wrong kind of table
  (let* ((new-preds (make-instance 'chilog:chilog-db-table
                                   :table-predicate #'chilog:chilog-predicate-p))
         (db (make-instance 'chilog:chilog-db)))
    (assert-error 'error
      (setf (chilog:variables db) new-preds)))

  ;; You cannot set just a single variable
  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (db (make-instance 'chilog:chilog-db)))
    (assert-error 'error
      (setf (chilog:variables db) X)))

  ;; You cannot set a non-variable for a variable
  (let* ((db (make-instance 'chilog:chilog-db)))
    (assert-error 'error
      (setf (chilog:variables db) 73))
    (assert-error 'error
      (setf (chilog:variables db) "foo")))

  ;;; setf add-predicate
  ;; You can set a whole table
  (let ((new-preds (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-predicate-p))
        (db (make-instance 'chilog:chilog-db)))
    (assert-false (eq (chilog:predicates db) new-preds))
    (setf (chilog:predicates db) new-preds)
    (assert-true (eq (chilog:predicates db) new-preds)))

  ;; You cannot set the wrong kind of table
  (let* ((new-vars (make-instance 'chilog:chilog-db-table
                                   :table-predicate #'chilog:chilog-variable-p))
         (db (make-instance 'chilog:chilog-db)))
    (assert-error 'error
      (setf (chilog:predicates db) new-vars)))

  ;; You cannot set just a single predicate
  (let* ((foo (make-instance 'chilog:chilog-predicate :name "foo" :arity 3))
         (db (make-instance 'chilog:chilog-db)))
    (assert-error 'error
      (setf (chilog:predicates db) foo)))

  ;; You cannot set a non-predicate for a predicate
  (let* ((db (make-instance 'chilog:chilog-db)))
    (assert-error 'error
      (setf (chilog:predicates db) 73))
    (assert-error 'error
      (setf (chilog:predicates db) "foo")))

  ;; add-variable!
  (let ((X (make-instance 'chilog:chilog-variable :name "X"))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (hash-table-count (chilog:variables db)))
    (chilog:add-variable! X db)
    (assert-number-equal 1
      (hash-table-count (chilog:variables db)))
    (assert-set-equal (list X)
      (alexandria:hash-table-values (chilog:variables db))))

  ;; Adding a raw string will create a chilog-variable for us and insert it.
  (let ((db (make-instance 'chilog:chilog-db))
        (x))
    (assert-number-equal 0
      (hash-table-count (chilog:variables db)))
    ;; add-variable! returns the variable we just inserted, so we can capture
    ;; that for later comparison.
    (setf x (chilog:add-variable! "crazy-Name!00" db))
    (assert-number-equal 1
      (hash-table-count (chilog:variables db)))
    (assert-set-equal (list x)
      (alexandria:hash-table-values (chilog:variables db))))

  ;; Adding a "malformed variable" should not work
  (let ((db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (hash-table-count (chilog:variables db)))
    (assert-error
     'error
     (chilog:add-variable!
      ;; We cannot add a predicate as a variable.
      (make-instance 'chilog:chilog-predicate :name "bad-pred" :arity 3) db))
    (assert-number-equal 0
      (hash-table-count (chilog:variables db)))
    (assert-set-equal '()
      (alexandria:hash-table-values (chilog:variables db))))

  ;; Adding the same variable multiple times should throw an exception
  ;; condition, preventing us from adding the same variable multiple times.
  (let ((X (make-instance 'chilog:chilog-variable :name "X"))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (hash-table-count (chilog:variables db)))
    (chilog:add-variable! X db)
    (assert-error
     'error
     (chilog:add-variable! X db))
    (assert-number-equal 1
      (hash-table-count (chilog:variables db)))
    (assert-set-equal (list X)
      (alexandria:hash-table-values (chilog:variables db))))

  ;; add-predicate!
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "pred"
                             :arity 3))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (hash-table-count (chilog:predicates db)))
    (chilog:add-predicate! pred db)
    (assert-number-equal 1
      (hash-table-count (chilog:predicates db)))
    (assert-set-equal (list pred)
      (alexandria:hash-table-values (chilog:predicates db))))

  ;; Adding a "malformed predicate" should not work
  (let ((db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (hash-table-count (chilog:predicates db)))
    (assert-error
     'error
     (chilog:add-predicate! "failing-predicate" db))
    (assert-number-equal 0
      (hash-table-count (chilog:predicates db)))
    (assert-set-equal '()
      (alexandria:hash-table-values (chilog:predicates db))))

  ;; Adding the same predicate multiple times should throw an exception
  ;; condition, preventing us from adding the same predicate multiple times.
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "pred"
                             :arity 3))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (hash-table-count (chilog:predicates db)))
    (chilog:add-predicate! pred db)
    (assert-error
     'error
     (chilog:add-predicate! pred db))
    (assert-number-equal 1
      (hash-table-count (chilog:predicates db)))
    (assert-set-equal (list pred)
      (alexandria:hash-table-values (chilog:predicates db))))
  )
