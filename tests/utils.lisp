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

(defun call-with-named-temporary-file
    (filename thunk &key (want-stream-p t) (want-fileobj-p t)
                      (direction :io)
                      (directory (uiop:temporary-directory))
                      keep)
  "Call THUNK with FILENAME specifying the name of the temporary file to use.

XXX: YOU are responsible for making sure the named temporary file does NOT
already exist!
If the temporary file you named already exists, then the file is opened and
EMPTIED, clearing the file!
TODO: Raise restartable condition if file already exists with options for the
user; like `uiop:with-output-file'?

The temporary file will be deleted once THUNK completes, unless the KEEP
argument when CALL-FUNCTION'ed returns true."
  (assert (or want-stream-p want-fileobj-p))
  (let* ((prefix-pn (uiop:ensure-absolute-pathname
                     (or (uiop:ensure-pathname directory
                                               :namestring :native
                                               :ensure-directory t
                                               :ensure-physical t)
                         #'uiop:temporary-directory)))
         (prefix-nns (uiop:native-namestring prefix-pn))
         (pathname (uiop:parse-native-namestring
                    (format nil "~A~A" prefix-nns filename))))
    (unwind-protect
         (progn
           (uiop:ensure-all-directories-exist (list pathname))
           (with-open-file (stream pathname
                                   :direction direction
                                   :if-exists :supersede
                                   :if-does-not-exist :create)
             (if want-fileobj-p
                 (uiop:call-function thunk stream pathname)
                 (uiop:call-function thunk stream))))
      (unless (uiop:call-function keep)
        (ignore-errors (uiop:delete-file-if-exists pathname))))))

;; TODO: Write a test for call-with-named-temporary-file

(defmacro with-named-temporary-file ((&key (stream (gensym "STREAM") streamp)
                                           (fileobj (gensym "FILEOBJ") fileobjp)
                                        filename
                                        directory keep direction)
                                     &body body)
  "Evaluate BODY where the symbols specified by keyword arguments STREAM and
FILEOBJ (if specified) are bound corresponding to the temporary file named by
FILENAME ready for I/O.

STREAM and FILEOBJ are symbols that are used inside of your BODY to represent
the opened file's stream and the file object itself.

This macro is similar to `uiop:with-temporary-file', where a temporary file is
created in DIRECTORY, except you must provide a FILENAME for the temporary file
to use. It is the caller's responsibility to ensure FILENAME is unique.

Some examples of this macro's usage are below.

(chil/tests:with-named-temporary-file (:stream fstream
                                       :filename \"test.txt\"
                                       :keep 't)
  (format fstream \"hello world\"))
(FLET ((#:WRAPPER421 (FSTREAM) (FORMAT FSTREAM \"hello world\")))
  (DECLARE (DYNAMIC-EXTENT (FUNCTION #:WRAPPER421)))
  (CHIL/TESTS:CALL-WITH-NAMED-TEMPORARY-FILE
    \"test.txt\"
    #'#:WRAPPER421
    :WANT-STREAM-P T
    :WANT-FILEOBJ-P NIL
    :KEEP 'T))

(chil/tests:with-named-temporary-file (:stream fstream
                                       :filename \"test.txt\"
                                       :fileobj fobj
                                       :keep '(lambda (k) k))
  (format fstream \"hello world\")
  (uiop:native-namestring fobj)
  'nil)
(FLET ((#:WRAPPER427 (FSTREAM FOBJ)
         (FORMAT FSTREAM \"hello world\")
         (UIOP/FILESYSTEM:NATIVE-NAMESTRING FOBJ)
         'NIL))
  (DECLARE (DYNAMIC-EXTENT (FUNCTION #:WRAPPER427)))
  (CHIL/TESTS:CALL-WITH-NAMED-TEMPORARY-FILE
    \"test.txt\"
    #'#:WRAPPER427
    :WANT-STREAM-P T
    :WANT-FILEOBJ-P T
    :KEEP '(LAMBDA (K) K)))"
  (check-type stream symbol "Stream must be given symbol to bind to in your body")
  (check-type fileobj symbol "File object used in BODY must be a symbol")
  (assert (or streamp fileobjp))
  (let* ((wrapperf (gensym "WRAPPER")))
    `(flet ((,wrapperf (,@(when streamp `(,stream))
                        ,@(when fileobjp `(,fileobj)))
              ,@body))
       (declare (dynamic-extent #',wrapperf))
       (call-with-named-temporary-file
        ,filename
        #',wrapperf
        :want-stream-p ,streamp
        :want-fileobj-p ,fileobjp
        ,@(when direction `(:direction ,direction))
        ,@(when directory `(:directory ,directory))
        ,@(when keep `(:keep ,keep))))))

;; NOTE: Ideally, we would write tests for the macro's expansion, but lisp-unit2
;; (and most other unit test libraries) don't offer this capability. So we will
;; just have to rely on this unit test being correct.
