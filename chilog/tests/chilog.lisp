(in-package :chilog/tests)

;; By this point, we should be able to MODEL the following Datalog program
;; expressed in a Pythonic syntax.

;; dl = Datalog()

;; parent = dl.predicate('parent', 2)
;; ancestor = dl.predicate('ancestor', 2)

;; X, Y, Z = dl.variable('X'), dl.variable('Y'), dl.variable('Z')

;; parent['alice', 'bob'] = ()
;; parent['bob', 'carol'] = ()
;; ancestor[X, Y] = parent[X, Y]
;; ancestor[X, Y] = parent[X, Z], ancestor[Z, Y]

;; NOTE: We cannot INTERPRET/EXECUTE/QUERY/INFER anything yet! We don't have
;; an interpreter set up yet.
