;;; TODO: Implement these tests as sanity checks!
;;; They are small enough that we can hand-check that the results are sensible
;;; while not tanking performance if we have a bad Datalog interpreter and/or
;;; query planner.

;;; Digraph path reachability
.decl edge(x : number, y : number)
edge(1,2).
edge(2,3).
edge(3,4).

.decl edge(x : number, y : number)
path(x,y) :- edge(x,y).
path(x,z) :- edge(x,y), path(y,z).
.output edge(IO=stdout)

;;; Two alternative but equivalent definitions of digraph path reachability to
;;; the recursive definition above.
path(x,z) :- path(x,y), path(y,z).
path(x,z) :- path(x,y), edge(y,z).

;;; Undirected edges (equivalence relations)
.decl path(x : number, y : number) eqrel
// or
path(x,y) :- path(y,x).
