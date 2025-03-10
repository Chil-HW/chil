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

(define-test verilator-example-sim-test ()
  (assert-number-equal 0
   (uiop:with-temporary-file (:stream tmp-stream
                              :pathname pname
                              :direction :output
                              ;; TODO: Only keep unit test files if the test
                              ;; fails.
                              :keep 't
                              :type "v")
     (let* ((inputs (list (make-instance 'verilog-net :name "x")
                          (make-instance 'verilog-net :name "y")))
            (outputs (list (make-instance 'verilog-net :name "f")))
            (binop (make-instance 'verilog-binop :op '+
                                                 :lhs (first inputs)
                                                 :rhs (second inputs)))
            (vmodule (make-instance 'verilog-module
                                    :name (chil/backends:hyphen->underscore
                                           "comb-binop")
                                    :inputs inputs
                                    :outputs outputs
                                    :body (make-instance 'verilog-assign
                                                         :target (first outputs)
                                                         :body binop))))
       ;; Turn off the DECLFILENAME lint flag, since we knowingly generate
       ;; Verilog files that do NOT have the same name as the module they
       ;; contain.
       (format tmp-stream "/* verilator lint_off DECLFILENAME */~%")
       (chil/backends/verilog:codegen vmodule tmp-stream)
       (format tmp-stream "/* verilator lint_off DECLFILENAME */~%")
       (uiop:finish-outputs tmp-stream)

       ;; FIXME: Replace run-program with launch-program for async handling and
       ;; parallel evaluation of tests. Need to use wait-process to get wait on
       ;; the new process to finish. wait-process behaves like waitpid, returning
       ;; the RC of the sub-process.
       (multiple-value-bind (stdout stderr rc)
           (uiop:run-program (list "verilator" "--lint-only"
                                   "-Wall"
                                   (uiop:native-namestring pname))
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
         rc)))))
