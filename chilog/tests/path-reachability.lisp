(defpackage :chilog/tests/digraph
  (:use :cl :lisp-unit2
        :chilog :chilog/tests/utils))

(in-package :chilog/tests/digraph)

;;; They are small enough that we can hand-check that the results are sensible
;;; while not tanking performance if we have a bad Datalog interpreter and/or
;;; query planner.

;;; Digraph path reachability
;; .decl edge(x : number, y : number)
;; edge(1,2).
;; edge(2,3).
;; edge(3,4).

;; .decl path(x : number, y : number)
;; path(x,y) :- edge(x,y).
;; path(x,z) :- edge(x,y), path(y,z).
;; .output path(IO=stdout)

;; ;;; Two alternative but equivalent definitions of digraph path reachability to
;; ;;; the recursive definition above.
;; path(x,z) :- path(x,y), path(y,z).
;; path(x,z) :- path(x,y), edge(y,z).

;; ;;; Undirected edges (equivalence relations)
;; .decl path(x : number, y : number) eqrel
;; // or
;; path(x,y) :- path(y,x).

(define-test digraph-reachability ()
  ;; Each of the 3 digraph programs below are equivalent, but they use a
  ;; different recursive rule to learn all the paths.

  (let ((db (make-instance 'chilog-db))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z"))
        (edge (make-instance 'chilog-predicate :name "edge" :arity 2))
        (path (make-instance 'chilog-predicate :name "path" :arity 2)))
    (add-variable! X db)
    (add-variable! Y db)
    (add-variable! Z db)
    (add-predicate! edge db)
    (add-predicate! path db)
    (add-fact! '(1 2) edge)
    (add-fact! '(2 3) edge)
    (add-fact! '(3 4) edge)
    (add-rules! path (list X Y) (predicate->atom edge (list X Y)))
    ;; path(X,Z) :- edge(X,Y), path(Y,Z).
    (add-rules! path (list X Z)
                (predicate->atom edge (list X Y))
                (predicate->atom path (list Y Z)))
    (chilog/interpreter:infer db)
    (assert-set-equal
     '((1 2) (2 3) (3 4)
       (1 3) (2 4)
       (1 4))
     (facts (gethash "path" (chilog:table->hash-table (predicates db))))
     :test #'equal))

  (let ((db (make-instance 'chilog-db))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z"))
        (edge (make-instance 'chilog-predicate :name "edge" :arity 2))
        (path (make-instance 'chilog-predicate :name "path" :arity 2)))
    (add-variable! X db)
    (add-variable! Y db)
    (add-variable! Z db)
    (add-predicate! edge db)
    (add-predicate! path db)
    (add-fact! '(1 2) edge)
    (add-fact! '(2 3) edge)
    (add-fact! '(3 4) edge)
    (add-rules! path (list X Y) (predicate->atom edge (list X Y)))
    ;; path(X,Z) :- path(X,Y), path(Y,Z).
    (add-rules! path (list X Z)
                (predicate->atom path (list X Y))
                (predicate->atom path (list Y Z)))
    (chilog/interpreter:infer db)
    (assert-set-equal
     '((1 2) (2 3) (3 4)
       (1 3) (2 4)
       (1 4))
     (facts (gethash "path" (chilog:table->hash-table (predicates db))))
     :test #'equal))

  (let ((db (make-instance 'chilog-db))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z"))
        (edge (make-instance 'chilog-predicate :name "edge" :arity 2))
        (path (make-instance 'chilog-predicate :name "path" :arity 2)))
    (add-variable! X db)
    (add-variable! Y db)
    (add-variable! Z db)
    (add-predicate! edge db)
    (add-predicate! path db)
    (add-fact! '(1 2) edge)
    (add-fact! '(2 3) edge)
    (add-fact! '(3 4) edge)
    (add-rules! path (list X Y) (predicate->atom edge (list X Y)))
    ;; path(X,Z) :- path(X,Y), edge(Y,Z).
    (add-rules! path (list X Z)
                (predicate->atom path (list X Y))
                (predicate->atom edge (list Y Z)))
    (chilog/interpreter:infer db)
    (assert-set-equal
     '((1 2) (2 3) (3 4)
       (1 3) (2 4)
       (1 4))
     (facts (gethash "path" (chilog:table->hash-table (predicates db))))
     :test #'equal))
  )

(define-test undigraph-reachability ()
  ;; This is similar to the digraph-reachability test, but uses the same graph
  ;; as above, but with undirected edges instead. This means that the recursive
  ;; rule has nearly double the number of inferred facts compared to the
  ;; directed variant.  However, most of those facts are "reflections" of one
  ;; another. The rule that expresses the undirected-ness of edges is an
  ;; "equivalence relation" and offers unique optimization opportunities.

  ;; The undirected graph is identical to the directed graph, but this program's
  ;; formulation of path/2 does NOT form a proper equivalence relation.
  (let ((db (make-instance 'chilog-db))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z"))
        (edge (make-instance 'chilog-predicate :name "edge" :arity 2))
        (path (make-instance 'chilog-predicate :name "path" :arity 2)))
    (add-variable! X db)
    (add-variable! Y db)
    (add-variable! Z db)
    (add-predicate! edge db)
    (add-predicate! path db)
    (add-fact! '(1 2) edge)
    (add-fact! '(2 3) edge)
    (add-fact! '(3 4) edge)
    ;; path(x,y) :- path(y,x).
    ;; NOTE: This rule means that a node can have an edge to itself!
    (add-rules! path (list X Y) (predicate->atom path (list Y X)))
    ;; path(x,y) :- edge(x,y).
    (add-rules! path (list X Y) (predicate->atom edge (list X Y)))
    ;; path(X,Z) :- edge(X,Y), path(Y,Z).
    (add-rules! path (list X Z)
                (predicate->atom edge (list X Y))
                (predicate->atom path (list Y Z)))
    (chilog/interpreter:infer db)
    (assert-set-equal
     '((1 2) (2 3) (3 4)
       (1 3) (2 4)
       (1 4)
       (2 1) (3 2) (4 3)
       (3 1) (4 2)
       (4 1)
       (1 1) (2 2) (3 3))
     (facts (gethash "path" (chilog:table->hash-table (predicates db))))
     :test #'equal))

  ;; Make the path predicate a full equivalence relation. This means that the
  ;; '(4 4) fact on path/2 will be captured as well.
  ;; Equivalence relations require: reflexivity, symmetry, and transitivity.
  (let ((db (make-instance 'chilog-db))
        (X (make-instance 'chilog-variable :name "X"))
        (Y (make-instance 'chilog-variable :name "Y"))
        (Z (make-instance 'chilog-variable :name "Z"))
        (edge (make-instance 'chilog-predicate :name "edge" :arity 2))
        (path (make-instance 'chilog-predicate :name "path" :arity 2)))
    (add-variable! X db)
    (add-variable! Y db)
    (add-variable! Z db)
    (add-predicate! edge db)
    (add-predicate! path db)
    (add-fact! '(1 2) edge)
    (add-fact! '(2 3) edge)
    (add-fact! '(3 4) edge)
    ;; Reflexivity
    ;; path(x, x) :- path(x, _).
    (add-rules! path (list X X)
                (predicate->atom path (list X (make-placeholder-variable))))
    ;; Symmetry
    ;; path(x,y) :- path(y,x).
    ;; NOTE: This rule means that a node can have an edge to itself!
    (add-rules! path (list X Y) (predicate->atom path (list Y X)))
    ;; path(x,y) :- edge(x,y).
    (add-rules! path (list X Y) (predicate->atom edge (list X Y)))
    ;; Transitivity
    ;; path(x,z) :- path(x,y), path(y,z).
    (add-rules! path (list X Z)
                (predicate->atom path (list X Y))
                (predicate->atom path (list Y Z)))
    ;; path(X,Z) :- edge(X,Y), path(Y,Z).
    (add-rules! path (list X Z)
                (predicate->atom edge (list X Y))
                (predicate->atom path (list Y Z)))
    (chilog/interpreter:infer db)
    (assert-set-equal
     '((1 2) (2 3) (3 4)
       (1 3) (2 4)
       (1 4)
       (2 1) (3 2) (4 3)
       (3 1) (4 2)
       (4 1)
       (1 1) (2 2) (3 3) (4 4))
     (facts (gethash "path" (chilog:table->hash-table (predicates db))))
     :test #'equal))
  )
