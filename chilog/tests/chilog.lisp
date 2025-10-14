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
     (or (integer)
         (real)
         (complex-generator (check-it:generator (real))
                            (check-it:generator (real)))))
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
      :test #'chilog:chilog-equal))

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
      :test #'chilog:chilog-equal))

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
      :test #'chilog:chilog-equal))

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
      :test #'chilog:chilog-equal))

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
    (assert-set-equal (remove-duplicates duplicated-facts
                                         :test #'chilog:chilog-equal)
      (chilog:facts pred)
      :test #'chilog:chilog-equal))

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
      :test #'chilog:chilog-equal))

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
      :test #'chilog:chilog-equal))

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
      :test #'chilog:chilog-equal))

  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 1))
         ;; Two structurally similar but distinct (according to eq) rules.
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
                                                         :terms (list X))
                                    :body (list
                                           (make-instance 'chilog:chilog-atom
                                                          :predicate "body"
                                                          :terms (list X)))))))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    (mapc (lambda (rule) (chilog:add-rule! rule pred))
        new-rules)
    (assert-number-equal 1
      (length (chilog:rules pred)))
    (assert-set-equal (remove-duplicates new-rules
                                         :test #'chilog:chilog-equal)
      (chilog:rules pred)
      :test #'chilog:chilog-equal))

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
    (mapc (lambda (rule) (chilog:add-rule! rule pred))
        new-rules)
    (assert-number-equal 2
      (length (chilog:rules pred)))
    (assert-set-equal new-rules
      (chilog:rules pred)
      :test #'chilog:chilog-equal))

  ;; A rule must use every argument in the head
  (let ((X (make-instance 'chilog:chilog-variable :name "X"))
        (Y (make-instance 'chilog:chilog-variable :name "Y"))
        (pred (make-instance 'chilog:chilog-predicate
                             :name "test"
                             :arity 2)))
    (assert-number-equal 0
      (length (chilog:rules pred)))
    (assert-error 'error
      (chilog:add-rules! pred (list X Y)
                         (predicate->atom pred (list X X)))))
  )

;; Despite #'predicate->atom being attached as a method, it is so commonly used
;; that it deserves its own define-test.
(define-test predicate->atom ()
  (let* ((pred-name "test")
         (pred (make-instance 'chilog:chilog-predicate
                              :name pred-name
                              :arity 1))
         (X (make-instance 'chilog:chilog-variable :name "X"))
         (terms (list X))
         (the-atom))
    (setf the-atom (chilog:predicate->atom pred terms))
    (assert-true the-atom)
    (assert-equal pred-name (chilog:predicate the-atom))
    (assert-equal terms (chilog:terms the-atom) :test #'chilog:chilog-equal))

  ;; Arity mis-match should throw an error
  (let* ((pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 2))
         (X (make-instance 'chilog:chilog-variable :name "X"))
         (terms (list X)))
    (assert-error 'error
      (chilog:predicate->atom pred terms)))

  ;; All provided terms must actually be terms. Throw an error otherwise.
  (let* ((pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 2))
         (X (make-instance 'chilog:chilog-variable :name "X")))
    (assert-error 'error
      (chilog:predicate->atom pred (list X pred))))

  ;; Calling predicate->atom incorrectly should just break, even if things are
  ;; fine otherwise.
  (let* ((pred (make-instance 'chilog:chilog-predicate
                              :name "test"
                              :arity 1))
         (X (make-instance 'chilog:chilog-variable :name "X")))
    (assert-error 'error
      (chilog:predicate->atom X (list pred))))
  )

(define-test chilog-db-table ()
  ;; Constructing a brand-new empty table should always work.
  (assert-true
   (eval (make-instance 'chilog:chilog-db-table
                        :table-predicate #'chilog:chilog-variable-p)))
  (assert-true
   (eval (make-instance 'chilog:chilog-db-table
                        :table-predicate #'chilog:chilog-predicate-p)))

  ;; Creating a table without a predicate should error
  (assert-error 'error
    (eval (make-instance 'chilog:chilog-db-table)))

  ;; Creating an unsupported table type should error
  (assert-error 'error
    (eval (make-instance 'chilog:chilog-db-table
                         :table-predicate #'chilog:chilog-rule-p)))

  ;; We do not allow you to provide a table at db-table construction-time.
  ;; This prevents you from installing already-known information, but means we
  ;; do not have to verify the data in the table is well-formed.
  (let ((ht (make-hash-table :test #'equal)))
    (assert-error 'error
      (make-instance 'chilog:chilog-db-table
                     :table ht
                     :table-predicate #'chilog:chilog-rule-p)))

  ;; A table must satisfy its own type/class/instanceof predicate.
  (assert-true
   (eval
    (chilog:chilog-db-table-p
     (make-instance 'chilog:chilog-db-table
                    :table-predicate #'chilog:chilog-variable-p))))

  ;; The chilog-equal predicate should hold for DB tables
  (let ((var-tab1 (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-variable-p))
        (var-tab2 (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-variable-p)))
    (assert-true
     (chilog:chilog-equal var-tab1 var-tab2)))
  (let ((pred-tab1 (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-predicate-p))
        (pred-tab2 (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-predicate-p)))
    (assert-true
     (chilog:chilog-equal pred-tab1 pred-tab2)))
  (let ((pred-tab (make-instance 'chilog:chilog-db-table
                                 :table-predicate #'chilog:chilog-predicate-p))
        (var-tab (make-instance 'chilog:chilog-db-table
                                :table-predicate #'chilog:chilog-variable-p)))
    (assert-false
     (chilog:chilog-equal pred-tab var-tab))
    (assert-false
     (chilog:chilog-equal var-tab pred-tab)))
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
      (chilog:table-count (chilog:variables db)))
    (chilog:add-variable! X db)
    (assert-number-equal 1
      (chilog:table-count (chilog:variables db)))
    (assert-set-equal (list X)
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:variables db)))))

  ;; Adding a raw string will create a chilog-variable for us and insert it.
  (let ((db (make-instance 'chilog:chilog-db))
        (x))
    (assert-number-equal 0
      (chilog:table-count (chilog:variables db)))
    ;; add-variable! returns the variable we just inserted, so we can capture
    ;; that for later comparison.
    (setf x (chilog:add-variable! "crazy-Name!00" db))
    (assert-number-equal 1
      (chilog:table-count (chilog:variables db)))
    (assert-set-equal (list x)
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:variables db)))))

  ;; Adding a "malformed variable" should not work
  (let ((db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (chilog:table-count (chilog:variables db)))
    (assert-error
     'error
     (chilog:add-variable!
      ;; We cannot add a predicate as a variable.
      (make-instance 'chilog:chilog-predicate :name "bad-pred" :arity 3) db))
    (assert-number-equal 0
      (chilog:table-count (chilog:variables db)))
    (assert-set-equal '()
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:variables db)))))

  ;; Adding the same variable multiple times should throw an exception
  ;; condition, preventing us from adding the same variable multiple times.
  (let ((X (make-instance 'chilog:chilog-variable :name "X"))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (chilog:table-count (chilog:variables db)))
    (chilog:add-variable! X db)
    (assert-error
     'error
     (chilog:add-variable! X db))
    (assert-number-equal 1
      (chilog:table-count (chilog:variables db)))
    (assert-set-equal (list X)
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:variables db)))))

  ;; add-predicate!
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "pred"
                             :arity 3))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (chilog:table-count (chilog:predicates db)))
    (chilog:add-predicate! pred db)
    (assert-number-equal 1
      (chilog:table-count (chilog:predicates db)))
    (assert-set-equal (list pred)
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:predicates db)))))

  ;; Adding a "malformed predicate" should not work
  (let ((db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (chilog:table-count (chilog:predicates db)))
    (assert-error
     'error
     (chilog:add-predicate! "failing-predicate" db))
    (assert-number-equal 0
      (chilog:table-count (chilog:predicates db)))
    (assert-set-equal '()
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:predicates db)))))

  ;; Adding the same predicate multiple times should throw an exception
  ;; condition, preventing us from adding the same predicate multiple times.
  (let ((pred (make-instance 'chilog:chilog-predicate
                             :name "pred"
                             :arity 3))
        (db (make-instance 'chilog:chilog-db)))
    (assert-number-equal 0
      (chilog:table-count (chilog:predicates db)))
    (chilog:add-predicate! pred db)
    (assert-error
     'error
     (chilog:add-predicate! pred db))
    (assert-number-equal 1
      (chilog:table-count (chilog:predicates db)))
    (assert-set-equal (list pred)
      (alexandria:hash-table-values
       (chilog:table->hash-table (chilog:predicates db)))))
  )

(define-test chilog-equal ()
  ;;; Sanity checking chilog-equal for non-Chilog values/objects
  ;; Verify that #'chilog-equal is "delete"d for normal Common Lisp values
  ;; Since these simple tests can have inference performed on them, we need to
  ;; prevent compile-time checks & evaluation from invalidating the tests.
  (assert-false
   (eval (chilog:chilog-equal 32 "hello")))
  (assert-false
   (eval
    (chilog:chilog-equal 32
                         (make-instance 'chilog:chilog-variable :name "X"))))
  (assert-false
   (eval
    (chilog:chilog-equal (make-instance 'chilog:chilog-variable :name "X")
                         #C(3 8.2))))
  ;; But we should not "delete" nil comparisons, since we need those to work for
  ;; map-style operations.
  (assert-true
   (eval (chilog:chilog-equal 'nil 'nil)))

  ;;; chilog-variable
  ;; Variables MUST be unique, pretty much according to #'eq rules (pointers are
  ;; the same). We have other tools to ensure a Chilog program only has one of
  ;; each variable.
  (let ((X1 (make-instance 'chilog:chilog-variable :name "X"))
        (X2 (make-instance 'chilog:chilog-variable :name "X")))
    (assert-true (chilog:chilog-equal X1 X1))
    (assert-true (chilog:chilog-equal X2 X2))
    (assert-false (chilog:chilog-equal X1 X2)))

  ;;; chilog-atom
  (let ((a1 (make-instance 'chilog:chilog-atom
                           :predicate "foo"
                           :terms '()))
        (a2 (make-instance 'chilog:chilog-atom
                           :predicate "foo"
                           :terms '())))
    (assert-true (chilog:chilog-equal a1 a1))
    (assert-true (chilog:chilog-equal a2 a2))
    (assert-true (chilog:chilog-equal a1 a2)))

  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (a1 (make-instance 'chilog:chilog-atom
                           :predicate "foo"
                           :terms `(,X)))
        (a2 (make-instance 'chilog:chilog-atom
                           :predicate "foo"
                           :terms (list X))))
    (assert-true (chilog:chilog-equal a1 a1))
    (assert-true (chilog:chilog-equal a2 a2))
    (assert-true (chilog:chilog-equal a1 a2)))

  ;; It is impossible to construct 2 atoms with different predicate names or
  ;; terms that are equivalent according to #'eq, since that requires two calls
  ;; to make-instance. So we expect #'chilog-equal to return false.
  (let* ((X (make-instance 'chilog:chilog-variable :name "X"))
         (a1 (make-instance 'chilog:chilog-atom
                            :predicate "foo"
                            :terms `("hello" ,X)))
         (a2 (make-instance 'chilog:chilog-atom
                            :predicate "foo"
                            :terms '(32))))
    (assert-true (chilog:chilog-equal a1 a1))
    (assert-true (chilog:chilog-equal a2 a2))
    (assert-false (chilog:chilog-equal a1 a2)))

  ;;; chilog-rule
  (let* ((head (make-instance 'chilog:chilog-atom
                              :predicate "head"
                              :terms '("foo" "bar")))
         (r1 (make-instance 'chilog:chilog-rule
                            :head head
                            :body '()))
         (r2 (make-instance 'chilog:chilog-rule
                            :head head
                            :body '())))
    (assert-true (chilog:chilog-equal r1 r1))
    (assert-true (chilog:chilog-equal r2 r2))
    (assert-true (chilog:chilog-equal r1 r2)))

  ;;; chilog-predicate

  ;;; chilog-db
  ;; Two databases that are empty should be equivalent
  (let ((db1 (make-instance 'chilog:chilog-db))
        (db2 (make-instance 'chilog:chilog-db)))
    (assert-true (chilog:chilog-equal db1 db1))
    (assert-true (chilog:chilog-equal db2 db2))
    (assert-true (chilog:chilog-equal db1 db2)))

  ;; Two databases with the same variables should be equivalent
  ;; There are 2 cases:
  ;;   1. The SAME variable (according to #'eq) is in both DBs.
  ;;   2. Two variables with the same string name is in both DBs.
  ;; For now, we treat the second case as something we do NOT want to allow.
  ;; We don't want to get into the case where we have to figure out if two
  ;; variables in different databases are actually the same.
  (let ((db1 (make-instance 'chilog:chilog-db))
        (db2 (make-instance 'chilog:chilog-db))
        (X (make-instance 'chilog:chilog-variable :name "X")))
    (assert-true
     (and (zerop (chilog:table-count (chilog:variables db1)))
          (zerop (chilog:table-count (chilog:variables db2)))))

    (chilog:add-variable! X db1)
    (chilog:add-variable! X db2)

    (assert-true
     (and (= 1 (chilog:table-count (chilog:variables db1)))
          (= 1 (chilog:table-count (chilog:variables db2)))))
    (assert-true
     (chilog:chilog-equal db1 db2)))

  (let ((db1 (make-instance 'chilog:chilog-db))
        (db2 (make-instance 'chilog:chilog-db))
        (X1 (make-instance 'chilog:chilog-variable :name "X"))
        (X2 (make-instance 'chilog:chilog-variable :name "X")))
    (assert-true
     (and (zerop (chilog:table-count (chilog:variables db1)))
          (zerop (chilog:table-count (chilog:variables db2)))))

    (chilog:add-variable! X1 db1)
    (chilog:add-variable! X2 db2)

    (assert-true
     (and (= 1 (chilog:table-count (chilog:variables db1)))
          (= 1 (chilog:table-count (chilog:variables db2)))))
    (assert-false
     (chilog:chilog-equal db1 db2)))

  ;; Two databases with the same predicates should be equivalent
  ;; Like variables, there are 2 cases:
  ;;   1. The SAME predicate (according to #'eq) is in both DBs.
  ;;   2. Two predicates with the same names and arity are in both DBs.
  ;; Similar to variables, we are not going to try to handle two distinct
  ;; predicates (according to #'eq) that "just so happen" to have the same name
  ;; and arity.
  (let ((db1 (make-instance 'chilog:chilog-db))
        (db2 (make-instance 'chilog:chilog-db))
        (pred (make-instance 'chilog:chilog-predicate :name "test-pred"
                                                      :arity 4)))
    (assert-true
     (and (zerop (chilog:table-count (chilog:predicates db1)))
          (zerop (chilog:table-count (chilog:predicates db2)))))

    (chilog:add-predicate! pred db1)
    (chilog:add-predicate! pred db2)

    (assert-true
     (and (= 1 (chilog:table-count (chilog:predicates db1)))
          (= 1 (chilog:table-count (chilog:predicates db2)))))
    (assert-true
     (chilog:chilog-equal db1 db2)))

  (let ((db1 (make-instance 'chilog:chilog-db))
        (db2 (make-instance 'chilog:chilog-db))
        (pred1 (make-instance 'chilog:chilog-predicate :name "test-pred"
                                                       :arity 4))
        (pred2 (make-instance 'chilog:chilog-predicate :name "test-pred"
                                                       :arity 4)))
    (assert-true
     (and (zerop (chilog:table-count (chilog:predicates db1)))
          (zerop (chilog:table-count (chilog:predicates db2)))))

    (chilog:add-predicate! pred1 db1)
    (chilog:add-predicate! pred2 db2)

    (assert-true
     (and (= 1 (chilog:table-count (chilog:predicates db1)))
          (= 1 (chilog:table-count (chilog:predicates db2)))))
    (assert-true
     (chilog:chilog-equal db1 db2)))

  ;; 2 databases with different contents that are STRUCTURALLY the same SHOULD
  ;; be equal, but only if they share the same variables!
  (let ((db1 (make-instance 'chilog:chilog-db))
        (db2 (make-instance 'chilog:chilog-db))
        (db3 (make-instance 'chilog:chilog-db))
        (X1 (make-instance 'chilog:chilog-variable :name "X"))
        (X2 (make-instance 'chilog:chilog-variable :name "X"))
        (pred1 (make-instance 'chilog:chilog-predicate :name "test-pred"
                                                       :arity 4))
        (pred2 (make-instance 'chilog:chilog-predicate :name "test-pred"
                                                       :arity 4)))
    (assert-true
     (and (zerop (chilog:table-count (chilog:variables db1)))
          (zerop (chilog:table-count (chilog:variables db1)))
          (zerop (chilog:table-count (chilog:predicates db1)))
          (zerop (chilog:table-count (chilog:predicates db2)))))

    (chilog:add-variable! X1 db1)
    (chilog:add-variable! X1 db3)
    (chilog:add-variable! X2 db2)
    (chilog:add-predicate! pred1 db1)
    (chilog:add-predicate! pred1 db3)
    (chilog:add-predicate! pred2 db2)

    (assert-true
     (and (= 1 (chilog:table-count (chilog:variables db1)))
          (= 1 (chilog:table-count (chilog:variables db3)))
          (= 1 (chilog:table-count (chilog:variables db2)))
          (= 1 (chilog:table-count (chilog:predicates db1)))
          (= 1 (chilog:table-count (chilog:predicates db3)))
          (= 1 (chilog:table-count (chilog:predicates db2)))))
    (assert-false
     (chilog:chilog-equal db1 db2))
    (assert-true
     (chilog:chilog-equal db1 db3)))
  )
