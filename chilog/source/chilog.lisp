(defpackage :chilog
  (:use :cl)
  (:export #:hello))

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
  (:documentation "A Chilog symbol/variable."))

(defun chilog-variable-p (v)
  (typep v (find-class 'chilog-variable)))

(defun chilog-value-p (v)
  (or (typep v 'boolean)
      ;; NOTE: numberp also allows complex values! If we need to refine to just
      ;; integer and floating-point, we need to add additional predicates.
      (numberp v)
      (stringp v)))

(deftype chilog-value ()
  `(satisfies chilog-value-p))

(defun chilog-term-p (term)
  (or (chilog-value-p term)
      (chilog-variable-p term)))

(deftype chilog-term ()
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
  (:documentation "A Chilog Atom.
What we call an `atom' is the USAGE of a predicate, e.g. \"father(X, bill)\"."))

(defun chilog-atom-p (a)
  (typep a (find-class 'chilog-atom)))

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
  (:documentation "A Chilog rule.
Chilog rules correspond to the :- components of Datalog.
The left-hand-side (LHS) of the :- is the head atom.
The right-hand-side (RHS) of the :- is the body list of atoms."))

(defun chilog-rule-p (r) (typep r (find-class 'chilog-rule)))

(defun chilog-fact-p (vals)
  (and (listp vals)
       (every #'chilog-value-p vals)))

(deftype chilog-fact ()
  "Facts are a \"row\" of `chilog-value's that belong to a single
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
    :documentation "Name for the predicate")
   (arity
    :reader arity
    :initarg :arity
    :initform (error "Chilog predicates must have a known non-negative arity")
    :type unsigned-byte
    :documentation "Arity of the predicate.
Must be >=0.")
   (facts
    :reader facts
    :writer add-fact!
    :initarg :facts
    :initform '()
    :type list-of-chilog-facts
    :documentation "Set of Chilog facts for this predicate")
   (rules
    :reader rules
    :writer add-rule!
    :initarg :rules
    :initform '()
    :type list-of-chilog-rules
    :documentation "List of Chilog rules for this predicate"))
  (:documentation "A Chilog predicate"))

(defmethod (setf facts) (new-facts (pred chilog-predicate))
  "Set a new set of facts for PRED.

NOTE: This does NOT verify that the new facts are sane in any way!"
  (check-type new-facts list-of-chilog-facts)
  ;; We should not allow the new facts to have a duplicate element!
  (setf (slot-value pred 'facts) new-facts))

(defmethod add-fact! (new-fact (pred chilog-predicate))
  ;; (setf (facts pred) pred)
  ;; FIXME: add-fact! should not add the same fact multiple times!
  (setf (facts pred) (cons new-fact (facts pred))))

(defmethod (setf rules) (new-rules (pred chilog-predicate))
  "Set a new set of rules for PRED.

NOTE: This does NOT verify that the new rules are sane in any way!"
  (check-type new-rules list-of-chilog-rules)
  (setf (slot-value pred 'rules) new-rules))

(defmethod add-rule! (new-rule (pred chilog-predicate))
  (setf (rules pred) (cons new-rule (rules pred))))

(defun predicate->atom (pred terms)
  "Convert the \"bare\" predicate PRED to a `chilog-atom' by providing
semi-concrete TERMS that match the declared arity of the predicate.

By semi-concrete, we mean that the terms can have both constant literals and
`chilog-variable's."
  (if (= (length terms) (arity pred))
      (make-instance
       'chilog-atom
       :predicate (name pred)
       :terms terms)
      (error "Number of provided terms does not match the predicate's arity")))

(defun add-rules! (predicate terms rhs)
  "Define the \"body\" of PREDICATE by setting the predicates TERMS to be
equivalent to RHS.

For the documentation below, assume the following Datalog rule:
parent(alice, bob).

We can treat this as syntactic sugar for representing the fact
parent(alice, bob) :- .

The predicate here is \"parent\", the terms are \"alice\" and \"bob\" and the
RHS is empty (because this is a fact)."
  ;; NOTE: This match is exhaustive because atom and listp comprise the two
  ;; possible partitions of all values.
  (cond
    ;; (uiop:emptyp 'a) |- NIL. So we can check that we have a list of terms
    ;; on the RHS before moving on to see if it is a list at all.
    ((uiop:emptyp rhs)
     ;; TODO: Facts should never contain chilog-variables! Make sure to check that!
     (add-fact! predicate terms)
     "empty rhs - We got a fact!")
    ((listp rhs)
     (add-rule!
      (make-instance
       'chilog-rule
       :head (make-instance 'chilog-atom
                            :predicate (name predicate)
                            :terms terms)
       :terms rhs)
      predicate))
    ((atom rhs)
     (add-rule!
      (make-instance
       'chilog-rule
       :head (make-instance 'chilog-atom
                            :predicate (name predicate)
                            :terms terms)
       :terms (list rhs))
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
  (:documentation "Chilog's fact database."))

(defmethod add-variable! ((new-var chilog-variable) (db chilog-db))
  "Add NEW-VAR `chilog-variable' to DB, the `chilog-db'."
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
  "Add NEW-PRED to DB, the Chilog database (`chilog-db')."
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
