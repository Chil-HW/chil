(defsystem :chilog
  :author "Karl Hallsby <karl@hallsby.com>"
  :description "Datalog in Common Lisp for Chil"
  :pathname #p"chilog/source/"
  :components ((:file "chilog")
               (:file "interpreter"
                :depends-on ("chilog"))
  :depends-on (:alexandria :log4cl :trivia)
  :in-order-to ((test-op (test-op :chilog/tests))))

(defsystem :chilog/tests
  :depends-on (:chilog :lisp-unit2 :check-it)
  :pathname #p"chilog/tests/"
  :components ((:file "utils")
               (:file "chilog")
               (:file "interpreter")
               (:file "path-reachability"))
  :around-compile (lambda (next)
                    (proclaim '(optimize (debug 3) (safety 3) (speed 0)))
                    (funcall next)))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (find-system :chilog/tests))))
  (let ((packages (list (find-package :chilog/tests)
                        (find-package :chilog/tests/interpreter)
                        (find-package :chilog/tests/digraph))))
    (unless (null
             (uiop:symbol-call
              :lisp-unit2 :failed
              (uiop:symbol-call
               :lisp-unit2 :run-tests
               :package packages
               :name :chilog
               :run-contexts (find-symbol "WITH-SUMMARY-CONTEXT" :lisp-unit2))))
      (uiop:quit 1))))
