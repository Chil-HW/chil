(defpackage :chilog/interpreter
  (:use :cl :log4cl
        :chilog)
  (:export #:alist->substitution
           #:alist->substitutions
           #:unify
           #:infer
           #:query))

(in-package :chilog/interpreter)

;;;
;;; The substitution table that we will continuously update as we learn more
;;; about the problem and gather additional facts/inferences.
;;;

;; (deftype chilog-substitution ()
;;   hashtable chilog-variable chilog-value)

(defun alist->substitution (substitution)
  "Convert and return SUBSTITUTION from an alist to Chilog's interpreter's
notion of a variable substitution."
  ;; (check-type subs alist)
  (alexandria:alist-hash-table substitution :test #'equal))

(defun alist->substitutions (&rest subs)
  "Convert each substitution in SUBS from an alist to Chilog's interpreter's
notion of a variable substitution, returning them as a list."
  (check-type subs list)
  (mapcar #'alist->substitution subs))

;; substitution is a hash-table!
;; NOTE: substitution is expected to be passed by-reference, so that
;; modifications to substitution made inside unify are visible outside unify.
(declaim (ftype (function (chilog-atom chilog-fact hash-table) boolean) unify))
(defun unify (atom fact substitution)
  "Attempt to unify ATOM's terms with FACT. If ATOM is a variable, then use the
provided SUBSTITUTIONs to attempt to find a suitable value to substitute.

Returns `t' when unification succeeds with SUBSTITUTION modified with the unified
substitution, `nil' otherwise."
  ;; NOTE: This parallel list iteration is why an atom's terms and a fact must
  ;; have the same arity!
  (loop for term of-type chilog-term in (terms atom)
        for val of-type chilog-value in fact
        do (cond
             ((chilog-variable-p term)
              (multiple-value-bind (v present?) (gethash term substitution)
                (if (and present? (not (equal val v)))
                    (progn
                      (log:debug "UNIFICATION FAILED! DIFFERING VALUES! " substitution " != " v)
                      (return-from unify 'nil))
                    ;; XXX: It is important that the passed-in substitution hash-table
                    ;; be modified directly! We want pass-by-reference semantics, not
                    ;; pass-by-value here!
                    (setf (gethash term substitution) val))))
             ;; term was not a chilog-variable, so it must be a chilog-value and we
             ;; can compare them directly.
             ((not (equal term val))
              (progn
                (log:debug "UNIFICATION FAILED! " term " != " val)
                (return-from unify 'nil)))
             ;; term was not a chilog-variable and the values were not (not (equal ...))
             ;; which means they are, and unification has not found anything new.
             ((equal term val)
              (progn
                (log:trace "UNIFICATION Nothing! " term " = " val)
                'nil))
             ;; Below is dead code. The two cond branches above should handle
             ;; all terms (chilog-variables and chilog-values). This last case
             ;; should never happen.
             (t (error "Attempted to unify something other than a chilog-value or chilog-variable!"))))
  (log:debug "Unification successful! " substitution)
  't)

;; NOTE: Renamed from search because the CL standard defines #'search.
(defun search-db (chilog-db i atoms subs)
  "Search in CHILOG-DB for ATOMS that match the provided substitions, SUBS.
I is used to control how \"far\" in the ATOM list we are currently searching.

Returns a list of valid substitutions (as hash-tables) for ATOMS."
  (log:trace "Searching DB with " atoms " and " subs " Currently at search depth " i)
  (if (= i (length atoms))
      (progn
        (log:debug "Completed DFS for " atoms ". Return " subs)
        subs)
      (let* ((atom (nth i atoms))
             (atom-preds (gethash (predicate atom)
                                  (table->hash-table (predicates chilog-db)))))
        ;; Flatten the resulting list from searching the DB. The reasoning is
        ;; 2-fold:
        ;; 1. search-db returns lists and when search-db is called
        ;;    recursively, collect will collect the list of substitutions into
        ;;    another list layer. So the amount of recursion in a rule's search
        ;;    in the DB will produce a nested list-of-list-of-...-list-of
        ;;    hash-tables. Flatten flattens this out to a single list.
        ;; 2. By virtue of flatten-ing, any 'nil that occurs because a
        ;;    predicate's facts set is empty will be erased. This prevents
        ;;    DB searching when a rule's fact list is empty from returning a
        ;;    list of 'nil substitutions.
        (alexandria:flatten
         (loop for fact in (facts atom-preds)
               for new-sub = (alexandria:copy-hash-table subs)
               do (log:debug "Try another fact! Try to unify " atom " with " fact " while " new-sub)
               when (unify atom fact new-sub)
                 collect (search-db chilog-db (1+ i) atoms new-sub))))))

(defun evaluate (chilog-db atoms)
  "Evalute the ATOMS of a predicate (the right-hand side of a predicate) in
CHILOG-DB. Evaluation is done by searching and unifying to find a valid set of
substitutions for the variables in ATOMS.

Returns a list of valid substitutions (as hash-tables) for ATOMS.
Evaluate the provided list of ATOMS in CHILOG-DB to produce a set/list of
possible substitions."
  (log:trace "Evaluating " atoms)
  (let ((res (search-db chilog-db 0 atoms (make-hash-table :test #'equal))))
    (log:trace "Evaluation results: " res)
    res))

(defun infer (chilog-db)
  "Perform a fixed-point iteration \"inference\" on the Datalog program to
determine the complete universe of facts present in the program.

In particular, this implementation performs a depth-first walk of the
predicates, rules, and facts in the Datalog program while performing a
fixed-point iteration to produce the complete universe of facts present in the
program."
  (log:debug "Inferring")
  ;; NOTE: The "do" is not important here! I only have it so Emacs indents the
  ;; loop's body nicely.
  (loop do
    ;; new-facts is List[Tuple[Predicate, Fact]]!
    ;; We treat it as (list (cons chilog-predicate chilog-fact)), an alist.
    (let ((new-facts '()))
      (loop for predicate in (alexandria:hash-table-values
                              (table->hash-table (predicates chilog-db)))
            do
               (log:debug "Performing inference on " predicate)
               (loop for rule in (rules predicate) do
                 (log:debug predicate " has rule " rule "! Eval it!")
                 (loop for sub in (evaluate chilog-db (body rule)) do
                   ;; The substitution hash-table has a substitution for EVERY
                   ;; variable in the rule we just evaluated. However, we only
                   ;; want to hold onto variables that were mentioned in the
                   ;; rule's head. Collect those variable-values into an ordered
                   ;; list that matches the arity of the predicate and use that
                   ;; to determine if we have "new facts".
                   (let ((sub-facts
                           (loop for term in (terms (head rule))
                                 for fact = (if (chilog-variable-p term)
                                                (gethash term sub)
                                                term)
                                 collect fact)))
                     (if (not (member sub-facts (facts predicate) :test #'equal))
                         (progn
                           (log:debug "Got a new fact! " predicate " = " sub-facts)
                           (setf new-facts
                                 (acons predicate sub-facts new-facts)))
                         (log:debug predicate " = " sub-facts " already known. Do nothing!"))))))
      ;; If we did NOT find new facts (new-facts is empty), then we have reached
      ;; the fixed-point and are done. Otherwise, we discovered new facts, so we
      ;; add them to the database and continue through our fixed-point again.
      ;; Chose alexandria here for no particular reason.
      (if (alexandria:emptyp new-facts)
          (progn
            (log:debug "Finished inference!")
            (loop for pred being the hash-value of (table->hash-table
                                                    (predicates chilog-db))
                  for facts = (facts pred)
                  do (log:trace pred " = " facts))
            ;; Returns from outer loop
            (return))
          (loop for (p . f) in new-facts
                do (log:debug "Recording new fact! " p " = " f)
                   (add-fact! f p))))))

(defun query (chilog-db &rest atoms)
  "Query the Datalog program to return a set of valid substitutions for the
variables in the ATOMS provided.

This also serves as a wrapper around `evaluate' to allow for a variadic amount
of ATOMS.

NOTE: This will perform unification! But this unification will be relatively
fast since we require inference to be perofrmed before we allow queries to be
made. If you try to query before inference is performed, an INCOMPLETE set of
facts will be returned."
  (evaluate chilog-db atoms))
