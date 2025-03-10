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
                              :keep 't
                              ;; TODO: It would be nice to generate a nicer
                              ;; module file name from the name of the module.
                              ;; :prefix ((uiop:strcat "tmp" ...))
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
       ;; FIXME: Replace run-program with launch-program for async handling and
       ;; parallel evaluation of tests. Need to use wait-process to get wait on
       ;; the new process to finish. wait-process behaves like waitpid, returning
       ;; the RC of the sub-process.
       (chil/backends/verilog:codegen vmodule tmp-stream)
       (uiop:finish-outputs tmp-stream)
       (multiple-value-bind (stdout stderr rc)
           (uiop:run-program (list "verilator" "--lint-only"
                                   "-Wall"
                                   (uiop:native-namestring pname))
                             ;; Do not raise a continuable condition when
                             ;; verilator has a non-zero exit code, since this
                             ;; is a unit test and cannot do anything with them.
                             :ignore-error-status 't)
         ;; (declare (ignore stdout stderr))
         (format t "~a~%~%" stdout)
         (format t "~a" stderr)
         rc)))))
