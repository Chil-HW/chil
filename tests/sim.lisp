(in-package :chil/tests/verilog)


;;;
;;; Run a simulator for running tests
;;;

;; TODO: This should NOT be evaluated at compile time, but at runtime!
(defvar verilator-installed-p
  (chil/tests:program-installed-p "verilator")
  "Is Verilator installed?

This is used to control whether or not simulation-based unit tests should be
run.")

;;;
;;; Basic Lint Checks
;;;

(define-test verilator-passthrough-test ()
  (assert-number-equal 0
   (chil/tests:with-named-temporary-file (:stream fstream
                                          :filename (module-filename *combinatorial-passthrough-module*)
                                          :fileobj fobj
                               :direction :output
                               ;; Only keep unit test files if the test fails.
                               :keep '(lambda (rc) (not (zerop rc))))
     (chil/backends/verilog:codegen *combinatorial-passthrough-module* fstream)
     (uiop:finish-outputs fstream)

     (multiple-value-bind (stdout stderr rc)
         (uiop:run-program (list "verilator" "--lint-only"
                                 "-Wall"
                                 (uiop:native-namestring fobj))
                           :output :string
                           :error-output :string
                           ;; Do not raise a continuable condition when
                           ;; verilator has a non-zero exit code, since this
                           ;; is a unit test and cannot do anything with them.
                           :ignore-error-status 't)
       ;; Only print program's stdout/stderr when test program's rc != 0.
       (unless (zerop rc)
         (format t "~a~%~%" stdout)
         (format t "~a" stderr))
       ;; Lastly, return rc from this thunk to be given to the :keep function as
       ;; an argument.
       rc))))

(define-test verilator-binop-test ()
  (assert-number-equal 0
   (chil/tests:with-named-temporary-file (:stream fstream
                                          :filename (module-filename *combinatorial-binop-module*)
                                          :fileobj fobj
                               :direction :output
                               ;; Only keep unit test files if the test fails.
                               :keep '(lambda (rc) (not (zerop rc))))
     (chil/backends/verilog:codegen *combinatorial-binop-module* fstream)
     (uiop:finish-outputs fstream)

     (multiple-value-bind (stdout stderr rc)
         (uiop:run-program (list "verilator" "--lint-only"
                                 "-Wall"
                                 (uiop:native-namestring fobj))
                           :output :string
                           :error-output :string
                           ;; Do not raise a continuable condition when
                           ;; verilator has a non-zero exit code, since this
                           ;; is a unit test and cannot do anything with them.
                           :ignore-error-status 't)
       ;; Only print program's stdout/stderr when test program's rc != 0.
       (unless (zerop rc)
         (format t "~a~%~%" stdout)
         (format t "~a" stderr))
       rc))))

;;; TODO: Use property-testing to generate Verilog backend modules from Chil
;;; that Verilator allows to pass linting

(define-test verilator-property-sim-test ()
  (assert-number-equal 0
   (chil/tests:with-named-temporary-file (:stream fstream
                                          :filename (module-filename *combinatorial-binop-module*)
                                          :fileobj fobj
                                          :direction :output
                                          ;; Only keep unit test files if the test fails.
                                          :keep '(lambda (rc) (not (zerop rc))))
     ;; Turn off the DECLFILENAME lint flag, since we knowingly generate
     ;; Verilog files that do NOT have the same name as the module they
     ;; contain.
     (format fstream "/* verilator lint_off DECLFILENAME */~%")
     (chil/backends/verilog:codegen *combinatorial-binop-module* fstream)
     (format fstream "/* verilator lint_off DECLFILENAME */~%")
     (uiop:finish-outputs fstream)

     ;; FIXME: Replace run-program with launch-program for async handling and
     ;; parallel evaluation of tests. Need to use wait-process to get wait on
     ;; the new process to finish. wait-process behaves like waitpid, returning
     ;; the RC of the sub-process.
     (multiple-value-bind (stdout stderr rc)
         (uiop:run-program (list "verilator" "--lint-only"
                                 "-Wall"
                                 (uiop:native-namestring fobj))
                           :output :string
                           :error-output :string
                           ;; Do not raise a continuable condition when
                           ;; verilator has a non-zero exit code, since this
                           ;; is a unit test and cannot do anything with them.
                           :ignore-error-status 't)
       ;; Only print program's stdout/stderr when test program's rc != 0.
       (unless (zerop rc)
         (format t "~a~%~%" stdout)
         (format t "~a" stderr))
       rc))))
