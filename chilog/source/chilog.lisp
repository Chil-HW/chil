(defpackage :chilog
  (:use :cl)
  (:export #:chilog-equal
           #:chilog-variable
           #:chilog-variable-p
           #:name
           #:*placeholder-variable-counter*
           #:make-placeholder-variable
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
           #:chilog-predicate
           #:chilog-predicate-p
           #:name #:arity #:facts #:rules
           #:add-fact! #:add-rule!
           #:predicate->atom ; DOES THIS NEED TO BE EXPORTED?
           #:add-rules!
           #:chilog-db-table
           #:chilog-db-table-p
           #:table-predicate
           #:table-count
           #:table->hash-table
           #:chilog-db
           #:variables #:predicates
           #:add-variable! #:add-predicate!))

(in-package :chilog)

;;; NOTE: For now we have the following limitations, some of which can be
;;; loosened in the future:
;;;  1. Negation is not allowed
;;;  2. Complex terms are not allowed, i.e. p(f(x), y) is disallowed
;;;  3. Every variable in the head of a clause MUST be used in the clause.

(defgeneric chilog-equal (c1 c2)
  (:documentation "Return 't if C1 and C2 are equal. Chilog's equality
operation does STRUCTURAL equality. It does NOT check that the provide Chilog
objects are equivalent. This is roughly equivalent to Common Lisp's `equal'."))

(defmethod chilog-equal (c1 c2)
  "\"Default implementation\" for Chilog's notion of equality.
Since this is the most generic method, we fall back to `equal'."
  (equal c1 c2))


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

(defmethod chilog-equal ((c1 chilog-variable) (c2 chilog-variable))
  "Equality of Chilog variables is effectively an #'eq comparison, since all
top-level variables in a Datalog program must be unique."
  (and (eq c1 c2)
       (string-equal (name c1) (name c2))))

(defparameter *placeholder-variable-counter* 0
  "Counter for generating unique Chilog placeholder variables.")

(defun make-placeholder-variable (&optional
                                    (prefix "GEN-chilog-variable"))
  "Create and return a new, unique, and fresh Chilog variable.
Optionally, you can provide a string PREFIX that will be used to uniquify the
variable."
  (let ((n *placeholder-variable-counter*))
    (incf *placeholder-variable-counter*)
    (make-instance 'chilog-variable
                   :name (format nil "~a~a" prefix n))))

(defun chilog-value-p (v)
  (or (typep v 'boolean)
      ;; NOTE: numberp also allows complex values! If we need to refine to just
      ;; integer and floating-point, we need to add additional predicates.
      (numberp v)
      (stringp v)))

(deftype chilog-value ()
  "A Chilog value is a concretized and constant value.

Despite Chilog accepting and allowing numbers (integers, reals, and complex)
and Booleans, these should be treated as opaque constants. We explicitly DO NOT
support mathematical or logical operations on the Common Lisp values that
underly `chilog-value''s."
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

(defmethod chilog-equal ((c1 chilog-atom) (c2 chilog-atom))
  "Equality of `chilog-atom's mostly amounts to checking that the terms in the
atoom are the same."
  (and (string-equal (predicate c1) (predicate c2))
       (every (lambda (t1 t2) (chilog-equal t1 t2))
              (terms c1) (terms c2))))

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

(defmethod chilog-equal ((c1 chilog-rule) (c2 chilog-rule))
  (and (chilog-equal (head c1) (head c2))
       (every (lambda (b1 b2) (chilog-equal b1 b2))
              (body c1) (body c2))))

(defun list-of-chilog-rules-p (rules)
  (and (listp rules)
       (every #'chilog-rule-p rules)))

(deftype list-of-chilog-rules ()
  `(satisfies list-of-chilog-rules-p))

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

(defmethod initialize-instance :after ((pred chilog-predicate) &key)
  (with-slots ((name name)
               (arity arity))
      pred
    (assert
     (and (not (zerop (length name)))
          (> arity 0))
     ()
     "~s/~a is an invalid Chilog predicate construction!"
            name arity)))

(defun chilog-predicate-p (p)
  (typep p (find-class 'chilog-predicate)))

(defmethod print-object ((pred chilog-predicate) stream)
  (print-unreadable-object (pred stream :type t :identity t)
    (format stream "~a/~a" (name pred) (arity pred))))

(defmethod chilog-equal ((c1 chilog-predicate) (c2 chilog-predicate))
  (and (string-equal (name c1) (name c2))
       (= (arity c1) (arity c2))
       ;; We do not use facts to determine equality because two different
       ;; chilog-predicates that have different pre-installed facts but are
       ;; otherwise the same.
       ;; We have to go through inference to find all facts anyways.
       (every (lambda (r1 r2) (chilog-equal r1 r2))
              (rules c1) (rules c2))))

(defmethod (setf facts) (new-facts (pred chilog-predicate))
  "Set a new set of facts for PRED.

NOTE: This does NOT verify that the new facts are sane in any way!"
  (check-type new-facts list-of-chilog-facts)
  ;; We should not allow the new facts to have a duplicate element!
  (setf (slot-value pred 'facts) new-facts))

(defmethod add-fact! (new-fact (pred chilog-predicate))
  "Add a NEW-FACT to the `chilog-predicate' PRED. Returns the new state of the
known facts set.

NOTE: add-fact! does NOT add the same fact multiple times! A fact is considered
the same according to the #'equal function."
  (check-type new-fact chilog-fact)
  (assert (= (length new-fact) (arity pred)) ()
          "Facts must have the same length as the arity of their predicate")
  ;; XXX: add-fact! should not add the same fact multiple times! adjoin handles
  ;; this for us.
  (setf (facts pred) (adjoin new-fact (facts pred)
                             :test #'equal)))

(defmethod (setf rules) (new-rules (pred chilog-predicate))
  "Set a new set of rules for PRED.

NOTE: This does NOT verify that the new rules are sane in any way!"
  (check-type new-rules list-of-chilog-rules)
  (setf (slot-value pred 'rules) new-rules))

(defmethod add-rule! (new-rule (pred chilog-predicate))
  "Add a NEW-RULE to the `chilog-predicate' PRED. Returns the new state of the
known rules set.

NOTE: add-rule! does NOT add the same rule multiple times! A rule is considered
the same according to the #'equal function."
  (check-type new-rule chilog-rule)
  (setf (rules pred) (adjoin new-rule (rules pred)
                             :test #'chilog-equal)))

(defmethod predicate->atom ((pred chilog-predicate) terms)
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
  (assert (= (length terms) (arity pred)) (terms)
          "Number of provided terms does not match the predicate's arity")
  (assert (list-of-chilog-terms-p terms) (terms)
          "Provided terms must be a list of Chilog terms")
  (make-instance
   'chilog-atom
   :predicate (name pred)
   :terms terms))

;; "delete" the default method implementation
(defmethod predicate->atom (pred terms)
  (error "Only predicates can be converted to atoms!"))

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
RHS = (predicate->atom parent '(X, Y)) :- (predicate->atom parent '(X, Y))

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
        (assert (every (lambda (term) (not (chilog-variable-p term))) terms) ()
                "Facts can only have literals provided as terms")
        (add-fact! predicate terms))
      ;; The RHS is non-empty, add a rule with the RHS set as terms.
      (progn
        ;; NOTE: If terms is empty, (every #'foo '()) |- 't
        (assert (every (lambda (term) (term-used? term rhs)) terms) ()
                "A rule's body MUST use ALL variables in head's args")
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

(defclass chilog-db-table ()
  ((table
    :reader table
    :initform (make-hash-table :test #'equal)
    :documentation "A table")
   (table-predicate
    :reader table-predicate
    :initarg :table-predicate
    :initform (error "You must provide a table predicate")
    :documentation "Table's predicate"))
  (:documentation "A \"table\" in the database."))

(defmethod initialize-instance :after ((tbl-obj chilog-db-table) &key)
  (with-slots ((table table)
               (tbl-pred table-predicate))
      tbl-obj
    (assert (and
             ;; We only accept variable & predicate tables.
             (or (eq tbl-pred #'chilog-variable-p)
                 (eq tbl-pred #'chilog-predicate-p))
             ;; hash-table-test returns a symbol, NOT a function symbol.
             (eq (hash-table-test table) 'equal)
             ;; The actual table must obey the format we expect.
             (every (lambda (k v)
                      (and (stringp k)
                           (funcall tbl-pred v)))
                    (alexandria:hash-table-keys table)
                    (alexandria:hash-table-values table))))))

(defun chilog-db-table-p (db-table)
  (typep db-table (find-class 'chilog-db-table)))

(defmethod table-count ((db-table chilog-db-table))
  "Return the count for the number of entries in the provided DB-TABLE."
  (hash-table-count (table db-table)))

(defmethod table->hash-table ((db-table chilog-db-table))
  "Convert the provided DB-TABLE to a hash-table."
  (table db-table))

(defmethod chilog-equal ((c1 chilog-db-table) (c2 chilog-db-table))
  ;; Both tables must use the same predicate and every value in the two database
  ;; tables must be equivalent.
  (and (eq (table-predicate c1) (table-predicate c2))
       (every (lambda (v1 v2) (chilog-equal v1 v2))
              (alexandria:hash-table-values (table c1))
              (alexandria:hash-table-values (table c2)))))

;; NOTE: These hash tables are NOT thread-safe!
(defclass chilog-db ()
  ((variables
    :reader variables
    :writer add-variable!
    :initarg :variables
    :initform (make-instance 'chilog-db-table
                             :table-predicate #'chilog-variable-p)
    ;; We cannot use objects in a :type constructor, so we validate this slot
    ;; with the initialize-instance :after method.
    ;; :type chilog-db-table
    :documentation "The set of `chilog-variable's known to Chilog's
program/database.")
   (predicates
    :reader predicates
    :writer add-predicate!
    :initarg :predicates
    :initform (make-instance 'chilog-db-table
                             :table-predicate #'chilog-variable-p)
    ;; We cannot use objects in a :type constructor, so we validate this slot
    ;; with the initialize-instance :after method.
    ;; :type chilog-db-table
    :documentation "The set of `chilog-predicate's known to Chilog's
program/database."))
  (:documentation "Chilog's representation of a Datalog program. This contains
all the `chilog-variable's and `chilog-predicate's in the Chilog/Datalog program.

This also includes a Datalog inference engine, using an iterative fixed-point
depth-first search algorithm to find solutions."))

(defmethod initialize-instance :after ((db chilog-db) &key)
  (with-slots ((vars variables)
               (preds predicates))
      db
    (assert (and (chilog-db-table-p vars)
                 (chilog-db-table-p preds)))))

(defmethod chilog-equal ((c1 chilog-db) (c2 chilog-db))
  "Two Chilog databases are equal if they contain the same variables and they
contain the same predicates according to `chilog-equal'."
  (and (chilog-equal (variables c1) (variables c2))
       (chilog-equal (predicates c1) (predicates c2))))

(defmethod add-variable! ((new-var chilog-variable) (db chilog-db))
  "Register the `chilog-variable' NEW-VAR in the Datalog program and return it."
  (multiple-value-bind (_var present?)
      (gethash (name new-var) (table (variables db)))
    ;; We don't hold onto _var for any reason right now. We cannot use _var
    ;; in the setf because that is not the location of the hash-table.
    (declare (ignore _var))
    (assert (not present?) (new-var)
            "Variable ~a already present in database" new-var)
    (setf (gethash (name new-var) (table (variables db))) new-var)))

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
  (multiple-value-bind (_pred present?)
      (gethash (name new-pred) (table (predicates db)))
    (declare (ignore _pred))
    (assert (not present?) (new-pred)
            "Predicate ~a already present in database" new-pred)
    (setf (gethash (name new-pred) (table (predicates db))) new-pred)))

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
