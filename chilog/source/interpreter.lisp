(defpackage :chilog/interpreter
  (:use :cl :chilog)
  (:export #:unify))

(in-package :chilog/interpreter)

;;;
;;; The substitution table that we will continuously update as we learn more
;;; about the problem and gather additional facts/inferences.
;;;

;; (deftype chilog-substitution ()
;;   hashtable chilog-variable chilog-value)

;; substitution is a hash-table!
(defun unify (atom fact substitution)
  "Attempt to unify ATOM's terms with FACT. If ATOM is a variable, then use the
provided SUBSTITUTIONs to attempt to find a suitable value to substitute.

Returns `nil' when unification fails, `t' otherwise."
  ;; NOTE: This zip is why an atom's terms and a fact must have the same arity!
  ;; The cons construction does not generalize beyond 2 lists, but produces a
  ;; pair which we can trivially destructure.
  (let (;; (zipped-terms (mapcar #'cons (terms atom) fact))
        (zipped-terms (mapcar #'list (terms atom) fact)))
    (loop for new-term in zipped-terms do
      ;; TODO: Use pattern matching on the zipped-terms!
      (let ((term (car zipped-terms))
            (val  (cdr zipped-terms)))
        (cond
          ((chilog-variable-p term)
           (multiple-value-bind (_val present?) (gethash term substitution)
             (declare (ignore _val))
             (if (and t (not present?))
                 (return 'nil)
                 ;; XXX: It is important that the passed-in substitution hash-table
                 ;; be modified directly! We want pass-by-reference semantics, not
                 ;; pass-by-value here!
                 (setf (gethash term substitution) val))))
          ;; term was not a chilog-variable, so it must be a chilog-value and we
          ;; can compare them directly.
          ((not (equal term val))
           (return 'nil))
          ;; NOTE: chilog-terms are either chilog-values or chilog-variables.
          ;; Thus, a chilog-atom's list-of-chilog-terms should be perfectly
          ;; partitioned by the two cond branches above and this last case
          ;; should never happen.
          ;; TODO: Hint that the 't branch of cond is dead code?
          (t (error "Attempted to unify something other than a chilog-value or chilog-variable!")))))
    't))

;; NOTE: Renamed from search because the CL standard defines #'search.
(defun search-db (chilog-db i atoms subs)
  "Search in CHILOG-DB for ATOMS that match the provided substitions, SUBS.

This implementation operates in a lazy(?) depth-first search."
  (if (= i (length atoms))
      subs
      (let* ((atom (nth i atoms))
             (atom-preds (gethash (predicate atom) (predicates chilog-db))))
        (loop for fact in (facts atom-preds) do
          (let ((new-sub (alexandria:copy-hash-table subs)))
            (when (unify atom fact new-sub)
              (search-db chilog-db (1+ i) atoms new-sub)))))))

(defun evaluate (chilog-db atoms)
  "Evaluate the provided list of ATOMS in CHILOG-DB to produce a set/list of
possible substitions."
  (search-db chilog-db 0 atoms (make-hash-table)))

(defun infer (chilog-db)
  "Perform the fixed-point inference on CHILOG-DB to find all facts."
  ;; NOTE: The "do" is not important here! I only have it so Emacs indents the
  ;; loop's body nicely.
  (loop do
    ;; new-facts is List[Tuple[Predicate, Fact]]!
    ;; We treat it as (list (cons chilog-predicate chilog-fact)), an alist.
    (let ((new-facts '()))
      (loop
        for predicate in (alexandria:hash-table-values (predicates chilog-db)) do
          (loop for rule in (rules predicate) do
            (loop for sub in (evaluate chilog-db (body rule)) do
              (let* ((terms (terms (head rule)))
                     (fact (if (chilog-variable-p terms)
                               (gethash t sub)
                               terms)))
                (unless (member fact (facts predicate))
                  (acons predicate fact new-facts))))))
      ;; Chose alexandria here for no particular reason.
      (if (alexandria:emptyp new-facts)
          (return)
          (loop for new-fact in new-facts do
            ;; TODO: Use pattern matching instead of manual destructuring!
            (let ((p (car new-fact))
                  (f (cdr new-fact)))
              (add-fact! f p)))))))

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
