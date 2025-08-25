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
  "Unify the ATOM with FACT. If ATOM is a variable, then use the provided
SUBSTITUTIONs to attempt to find a suitable value to substitute."
  (let ((zipped-terms (mapcar #'list (terms atom) fact)))
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
                 (setf (gethash term substitution) val))))
          ((not (equal term val))
           (return 'nil))
          (t 'do-nothing))))
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

;; TODO: I wonder if the transducers package would make writing this inference
;; engine any nicer?
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
  "Wrapper around `evaluate' that allows for a variadic amount of ATOMS."
  (evaluate chilog-db atoms))
