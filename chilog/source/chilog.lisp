(defpackage :chilog
  (:use :cl)
  (:export #:chilog-variable
           #:chilog-variable-p
           #:name
           #:chilog-value
           #:chilog-value-p
           #:chilog-term
           #:chilog-term-p
           #:list-of-chilog-terms
           #:list-of-chilog-terms-p
           #:chilog-atom
           #:chilog-atom-p
           #:predicate #:terms
           #:list-of-chilog-atoms
           #:list-of-chilog-atoms-p
           #:chilog-rule
           #:chilog-rule-p
           #:head #:body
           #:list-of-chilog-rules
           #:list-of-chilog-rules-p
           #:chilog-fact
           #:chilog-fact-p
           #:list-of-chilog-facts
           #:list-of-chilog-facts-p
           #:list-of-chilog-rules
           #:list-of-chilog-rules-p
           #:chilog-predicate
           #:chilog-predicate-p
           #:name #:arity #:facts #:rules
           #:add-fact! #:add-rule!
           #:predicate->atom ; DOES THIS NEED TO BE EXPORTED?
           #:add-rules!
           #:chilog-db
           #:variables #:predicates
           #:add-variable! #:add-predicate!))

(in-package :chilog)

;;; NOTE: For now we have the following limitations, some of which can be
;;; loosened in the future:
;;;  1. Negation is not allowed
;;;  2. Complex terms are not allowed, i.e. p(f(x), y) is disallowed
;;;  3. Every variable in the head of a clause MUST be used in the clause.


;;;
;;; The various components and types of our modeling of the Datalog language,
;;; including variables, atoms, rules, and predicates.
;;;

(defclass chilog-variable ()
  ((name
    :reader name
    :initarg :name
    :initform (error "Variables MUST have names!")
    :type string
    :documentation "The variable's name as a string"))
  (:documentation "A Chilog symbol/variable.
The variable will take on a value once inference is performed by the Chilog
engine."))

(defun chilog-variable-p (v)
  (typep v (find-class 'chilog-variable)))

(defmethod print-object ((var chilog-variable) stream)
  (print-unreadable-object (var stream :type t :identity t)
    (format stream "~a" (name var))))

(defun chilog-value-p (v)
  (or (typep v 'boolean)
      ;; NOTE: numberp also allows complex values! If we need to refine to just
      ;; integer and floating-point, we need to add additional predicates.
      (numberp v)
      (stringp v)))

(deftype chilog-value ()
  "A Chilog value is a concretized and constant value."
  `(satisfies chilog-value-p))

(defun chilog-term-p (term)
  (or (chilog-value-p term)
      (chilog-variable-p term)))

(deftype chilog-term ()
  "A Chilog term is either a `chilog-variable' (X, Z, FOO) or a `chilog-value'
('t, 32, 2.3, or \"hello\"). `chilog-predicate's use `chilog-term's in their
\"argument lists\" either to define the predicate or turn a predicate into a
`chilog-atom' for another rule."
  `(satisfies chilog-term-p))

(defun list-of-chilog-terms-p (terms)
  (and (listp terms)
       (every #'chilog-term-p terms)))

(deftype list-of-chilog-terms ()
  `(satisfies list-of-chilog-terms-p))

(defclass chilog-atom ()
  ((predicate
    :reader predicate
    :initarg :predicate
    :initform (error "Atoms must have a predicate/head!")
    :type string
    :documentation "Predicate's name in the atom.
In our example for `atom''s documentation, the predicate would be \"father\".")
   (terms
    :reader terms
    :initarg :terms
    :initform (error "Atoms must have terms provided at construction!")
    :type list-of-chilog-terms
    :documentation "The list of terms that this `atom' uses.
In our example for `atom''s documentation, the terms are \"'(\"X\" \"bill\")\"."))
  (:documentation "A Chilog Atom is a usage of a `chilog-predicate'; an atom is
a predicate with `chilog-term's provided to it.

In the Datalog example below, ancestor[X, Y] AND parent[X, Y] are Atoms.
ancestor[X, Y] = parent[X, Y]

NOTE: An Atom's predicate is just the NAME of the predicate, NOT the actual
predicate object."))

(defun chilog-atom-p (a)
  (typep a (find-class 'chilog-atom)))

(defmethod print-object ((atom chilog-atom) stream)
  (let ((ts (format 'nil "~{~a~^, ~}" (terms atom))))
    (print-unreadable-object (atom stream :type t :identity t)
      (format stream "~a(~a)" (predicate atom) ts))))

(defun list-of-chilog-atoms-p (atoms)
  (and (listp atoms)
       (every #'chilog-atom-p atoms)))

(deftype list-of-chilog-atoms ()
  `(satisfies list-of-chilog-atoms-p))

(defclass chilog-rule ()
  ((head
    :reader head
    :initarg :head
    :initform (error "Chilog rules must have a head")
    :type chilog-atom
    :documentation "The head atom of the rule.")
   (body
    :reader body
    :initarg :body
    :initform (error "Chilog rules must have a non-empty body")
    :type list-of-chilog-atoms
    :documentation "The atoms that form the body of the rule."))
  (:documentation "A `chilog-rule' is a single Datalog equation.
In practice, this means `chilog-rule's correspond to the :- components of
Datalog.

Rules have a head and a body.
The left-hand-side (LHS) of the :- is the head atom.
The right-hand-side (RHS) of the :- is the body list of atoms."))

(defun chilog-rule-p (r) (typep r (find-class 'chilog-rule)))

(defmethod print-object ((rule chilog-rule) stream)
  (let ((body-str (format 'nil "~{~a~^, ~}" (body rule))))
    (print-unreadable-object (rule stream :type t :identity t)
      (format stream "~a :- ~a" (head rule) body-str))))

(defun chilog-fact-p (vals)
  (and (listp vals)
       (every #'chilog-value-p vals)))

(deftype chilog-fact ()
  "A `chilog-fact' is a completely concretized set of values that match the
arity of a `chilog-predicate' and satisfy the predicate's terms according to the
predicate's rules.

Informally, facts are a \"row\" of `chilog-value's that belong to a single
`chilog-predicate'."
  `(satisfies chilog-fact-p))

(defun list-of-chilog-facts-p (facts)
  (and (listp facts)
       (every #'chilog-fact-p facts)))

(deftype list-of-chilog-facts ()
  "A list of Chilog facts (`chilog-fact').
NOTE: A `chilog-fact' is itself a list, so a list of Chilog facts is a list of
lists!"
  `(satisfies list-of-chilog-facts-p))

(defun list-of-chilog-rules-p (rules)
  (and (listp rules)
       (every #'chilog-rule-p rules)))

(deftype list-of-chilog-rules ()
  `(satisfies list-of-chilog-rules-p))

(defclass chilog-predicate ()
  ((name
    :reader name
    :initarg :name
    :initform (error "Chilog predicates must have a name")
    :type string
    :documentation "Name for the predicate.
In our example from `chilog-predicate''s documentation, the name is \"ancestor\".")
   (arity
    :reader arity
    :initarg :arity
    :initform (error "Chilog predicates must have a known non-negative arity")
    :type unsigned-byte
    :documentation "Arity of the predicate.
Must be >=0. In our example from `chilog-predicate''s documentation, the arity
is 2.")
   (facts
    :reader facts
    :writer add-fact!
    :initarg :facts
    :initform '()
    :type list-of-chilog-facts
    :documentation "Set of Chilog facts for this predicate.
This is the set of things we know either because they were provided to us in the
Datalog program, or we \"learned\" them by evaluating the rules and unifying
values.
These are values that would show up on the right side of the :- in Datalog, but
are NOT calls to other predicates.")
   (rules
    :reader rules
    :writer add-rule!
    :initarg :rules
    :initform '()
    :type list-of-chilog-rules
    :documentation "List of Chilog rules for this predicate, the right-hand side
of the :- operator in Datalog.
In our example from `chilog-predicate''s documentation, there are two rules,
the \"parent(X, Z)\" and \"ancestor(Z, Y)\" rules."))
  (:documentation "A `chilog-predicate' is something that Chilog must solve
during inference.
Predicates consist of a name, the predicate's arity, and all rules that are
on the right-hand side of the :- symbol.

NOTE: Facts are also predicates! But, they have an empty set of rules.

Each predicate ALWAYS tracks any facts it knows from construction or learns
during inference. Once inference is complete, you can always access the
complete learned/inferred/provided set of facts.

The Datalog below represents what we call a predicate.
ancestor(X, Y) :- parent(X, Z), ancestor(Z, Y)"))

(defun chilog-predicate-p (p)
  (typep p (find-class 'chilog-predicate)))

(defmethod print-object ((pred chilog-predicate) stream)
  (print-unreadable-object (pred stream :type t :identity t)
    (format stream "~a/~a" (name pred) (arity pred))))

(defmethod (setf facts) (new-facts (pred chilog-predicate))
  "Set a new set of facts for PRED.

NOTE: This does NOT verify that the new facts are sane in any way!"
  (check-type new-facts list-of-chilog-facts)
  ;; We should not allow the new facts to have a duplicate element!
  (setf (slot-value pred 'facts) new-facts))

(defmethod add-fact! (new-fact (pred chilog-predicate))
  (check-type new-fact chilog-fact)
  ;; add-fact! should not add the same fact multiple times!
  (assert (not (member new-fact (facts pred) :test #'equalp)))
  (setf (facts pred) (cons new-fact (facts pred))))

(defmethod (setf rules) (new-rules (pred chilog-predicate))
  "Set a new set of rules for PRED.

NOTE: This does NOT verify that the new rules are sane in any way!"
  (check-type new-rules list-of-chilog-rules)
  (setf (slot-value pred 'rules) new-rules))

(defmethod add-rule! (new-rule (pred chilog-predicate))
  (setf (rules pred) (cons new-rule (rules pred))))

(defun predicate->atom (pred terms)
  "Convert the \"bare\" `chilog-predicate' PRED to a `chilog-atom' by providing
semi-concrete TERMS that match the declared arity of the predicate. By
semi-concrete, we mean that the terms can be either constant literals or
`chilog-variable's.

This is used in the construction of predicates. For example, the parent[X, Y]
in the Datalog equation below
ancestor(X, Y) :- parent(X, Y)

will have the variables X and Y applied as terms to each predicate to form a
`chilog-atom'. This atom is then the given to `add-rules!' to be included in the
database."
  (if (= (length terms) (arity pred))
      (make-instance
       'chilog-atom
       :predicate (name pred)
       :terms terms)
      (error "Number of provided terms does not match the predicate's arity")))

(declaim (ftype (function (chilog-term list-of-chilog-atoms) boolean) term-used?))
(defun term-used? (term rhs)
  "Return 't if TERM is used at least once in the list of atoms provided as the
RHS."
  (cond
    ((chilog-variable-p term)
     ;; Some atom on the RHS must have term as a member of the atom's terms.
     ;; foo(X) :- bar(X). is required.
     ;; foo(X) :- bar. is illegal. member ensures this behavior.
     ;; foo(X) :- bar(X, Y). is allowed.
     ;; NOTE: (member 'foo '()) |- 'nil
     (some (lambda (atom) (not (null (member term (terms atom) :test #'equal)))) rhs))
    ;; Terms are "always used", so return 't
    ((chilog-value-p term) 't)
    ;; Below is dead code. Everything is either a Chilog variable or value!
    (t (error "Unknown term type!"))))

(defun add-rules! (predicate terms &rest rhs)
  "Concretize PREDICATE into a rule/line of Datalog by setting the predicate's
arguments to be TERMS and those to be equivalent to RHS.

This is usually the function you want to use to add a rule to a predicate. It
provides additional checks and is generally more convenient to use.

This function also checks that every argument in the head, is used AT LEAST once
in the rule's body. In other words, the rule's body MUST refer to ALL specified
variables! This is a safety condition that prevents us from finding infinite
solutions to an equation which are not interesting, but would keep the Datalog
engine searching forever.

This is also how you define facts! A fact is a predicate that has
literals for TERMS and RHS is the empty tuple, denoting that it has no
equations to satisfy to be true.

For the documentation below, assume the following Datalog rules:
parent(alice, bob).
ancestor(X, Y) = parent(X, Y)

PREDICATE = ancestor
TERMS = (Variable X, Variable Y)
RHS = (predicate->atom parent (list X Y))

We can treat the first line as syntactic sugar for representing fact, giving us:
parent(alice, bob) :- .

Which means the RHS is empty, because \"parent(alice, bob) :- .\" is a fact."
  ;; NOTE: This match is exhaustive because atom and listp comprise the two
  ;; possible partitions of all values.
  (check-type rhs list-of-chilog-atoms
              "The RHS of a rule (:-) MUST be a list of atoms!")
  (if (uiop:emptyp rhs)
      ;; The RHS is empty, which means this must be a fact.
      (progn
        ;; Facts can only have literals provided as terms
        (assert (every (lambda (term) (not (chilog-variable-p term))) terms))
        (add-fact! predicate terms))
      ;; The RHS is non-empty, add a rule with the RHS set as terms.
      (progn
        ;; If a rule has argument terms in the head, the rule's
        ;; body MUST refer to ALL specified variables!
        ;; NOTE: If terms is empty, (every #'foo '()) |- 't
        (assert (every (lambda (term) (term-used? term rhs)) terms))
        (add-rule!
         (make-instance
          'chilog-rule
          :head (make-instance
                 'chilog-atom
                 :predicate (name predicate)
                 :terms terms)
          :body rhs)
         predicate))))


;;;
;;; The database we will use as our first test of Datalog facts that we will
;;; iteratively grow until saturation to answer the desired logic question.
;;;

;; NOTE: These hash tables are NOT thread-safe!
(defclass chilog-db ()
  ((variables
    :reader variables
    :writer add-variable!
    :initarg :variables
    :initform (make-hash-table :test #'equal)
    ;; :type map-str-chilog-variable
    ;; :type string
    :documentation "The set of `chilog-variable's known to Chilog's
program/database.")
   (predicates
    :reader predicates
    :writer add-predicate!
    :initarg :predicates
    :initform (make-hash-table :test #'equal)
    ;; :type map-str-chilog-predicate
    ;; :type hash-table
    :documentation "The set of `chilog-predicate's known to Chilog's
program/database."))
  (:documentation "Chilog's representation of a Datalog program. This contains
all the `chilog-variable's and `chilog-predicate's in the Chilog/Datalog program.

This also includes a Datalog inference engine, using an iterative fixed-point
depth-first search algorithm to find solutions."))

(defmethod add-variable! ((new-var chilog-variable) (db chilog-db))
  "Register the `chilog-variable' NEW-VAR in the Datalog program and return it."
  (restart-case
      (multiple-value-bind (_var present?)
          (gethash (name new-var) (variables db))
        ;; We don't hold onto _var for any reason right now. We cannot use _var
        ;; in the setf because that is not the location of the hash-table.
        (declare (ignore _var))
        (if present?
            ;; This error lets us go to the restart.
            (error "Variable already present in database.")
            (setf (gethash (name new-var) (variables db)) new-var)))

    (set-new-var (name)
      :report "New variable name"
      :interactive (lambda ()
                     (format *query-io* "New variable name: ")
                     (force-output *query-io*)
                     ;; The eval lets us take in a make-instance call and turn
                     ;; it into an actual object, as opposed to just a list.
                     (list (eval (read *query-io*))))
      (add-variable! name db))))

(defmethod add-variable! ((var-name string) (db chilog-db))
  "Add VAR-NAME to DB, by creating the `chilog-variable' for you and then
inserting it into the `chilog-db'."
  (let ((v (make-instance 'chilog-variable :name var-name)))
    (add-variable! v db)))

;; "delete" the default :writer method that CLOS created.
(defmethod add-variable! (var-name (db chilog-db))
  (error "You can only add variables to the database as strings or variables!"))

(defmethod (setf variables) ((new-vars chilog-db-table) (db chilog-db))
  "Set NEW-VARS to be the variable table in the Chilog DB.
Returns NEW-VARS on success.
NOTE: This does NOT verify that the new variables are sane!"
  (assert (eq (table-predicate new-vars) #'chilog-variable-p) (new-vars)
          "The new table must be a variable table")
  (setf (slot-value db 'variables) new-vars))

(defmethod (setf variables) (new-vars (db chilog-db))
  (error "Cannot set DB variables with unknown type of ~a" new-vars))

(defmethod add-predicate! ((new-pred chilog-predicate) (db chilog-db))
  "Register the `chilog-predicate' NEW-PRED with the Datalog program and return
it."
  (restart-case
      (multiple-value-bind (_pred present?)
          (gethash (name new-pred) (predicates db))
        (declare (ignore _pred))
        (if present?
            ;; This error lets us go to the restart.
            (error "Predicate already present in database.")
            (setf (gethash (name new-pred) (predicates db)) new-pred)))

    (set-new-pred (restart-pred)
      :report "Give new predicate"
      :interactive (lambda ()
                     (format *query-io* "New predicate object: ")
                     (force-output *query-io*)
                     ;; The eval lets us take in a make-instance call and turn
                     ;; it into an actual object, as opposed to just a list.
                     (list (eval (read *query-io*))))
      (add-predicate! restart-pred db))))

;; "delete" the default :writer method that CLOS created.
(defmethod add-predicate! (new-pred (db chilog-db))
  (error "You can only add chilog-predicates to the database"))

(defmethod (setf predicates) ((new-preds chilog-db-table) (db chilog-db))
  "Set NEW-PREDS to be the predicate table in the Chilog DB.
Returns NEW-PREDS on success.
NOTE: This does NOT verify that the new predicates are sane!"
  (assert (eq (table-predicate new-preds) #'chilog-predicate-p) (new-preds)
          "The new table must be a predicate table")
  (setf (slot-value db 'predicates) new-preds))

(defmethod (setf predicates) (new-preds (db chilog-db))
  (error "Cannot set DB predicates with unknown type of ~a" new-preds))
