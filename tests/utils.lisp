(in-package :chil/tests)

(defun program-installed-p (prog-name)
  "Determine if the program with binary PROG-NAME is installed and available to
the Lisp image. PROG-NAME is expected to be a string that is present in `$PATH'.

Return `nil' if the program is NOT installed, `t' otherwise."
  (multiple-value-bind (stdout stderr rc)
      (handler-case (uiop:run-program prog-name
                                      ;; Ignore cases where prog-name is
                                      ;; installed but we called it wrong.
                                      :ignore-error-status 't)
        (error (condition)
          (declare (ignore condition))
          (log4cl:log-warn "~a is NOT installed!~%" prog-name)
          'nil))
    (declare (ignore stdout stderr))
    (zerop rc)))
