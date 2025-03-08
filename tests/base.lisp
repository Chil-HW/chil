(defpackage :chil/tests
  (:use :cl :lisp-unit2 :check-it)
  (:export #:chil-symbol-generator
           #:program-installed-p))

(in-package :chil/tests)


;;;
;;; A set of useful tools for all backend unit tests
;;;

;; A check-it generator to build valid Chil symbols.
;; This is a decently complex generator because check-it does not have a
;; way to produce a string generator based on a range of symbols. The
;; default string generator only generates strings drawn from the
;; regexps [a-zA-Z0-9]* (or [[:alnum:]]*).
;; To create this generator, we must manually create an or-generator for
;; each of the possible kinds of characters we can generate, wrap that
;; with a list generator to collect the resulting characters, then use a
;; map generator to convert that list of characters to a string. We then
;; double-check that what we produce is actually a valid Chil symbol by
;; using the guard generator.
(check-it:def-generator chil-symbol-generator ()
  (check-it:generator
   ;; Guard to make sure the string is a valid Chil string
   (check-it:guard (lambda (symbol)
                     (multiple-value-bind (substr-match regs)
                         (cl-ppcre:scan-to-strings chil/ir:chil-symbol-regexp symbol)
                       (declare (ignore regs))
                       ;; If the matched string is the same as the provided
                       ;; symbol, then the generated string IS actually a valid
                       ;; Chil symbol.
                       (string= symbol substr-match)))
    ;; Map the generated list of characters to a proper string
    (map
     (lambda (cs) (concatenate 'string cs))
     ;; Generate a list of valid Chil characters
     (list
      (or
       ;; For now we assume ASCII characters. UTF might be added in the future.
       ;; The alphanumerics
       ;; [[:alnum:]]
       (character #.(char-code #\a)
                  #.(char-code #\z))
       (character #.(char-code #\A)
                  #.(char-code #\Z))
       (character #.(char-code #\0)
                  #.(char-code #\9))
       ;; The symbols
       ;; [#$%&]
       (character #.(char-code #\#)
                  #.(char-code #\&))
       ;; [<=>?@]
       (character #.(char-code #\<)
                  #.(char-code #\@))
       ;; [^_]
       (character #.(char-code #\^)
                  #.(char-code #\_))
       ;; [\-]
       (character #.(char-code #\-)
                  #.(char-code #\-))
       ;; [/]
       (character #.(char-code #\/)
                  #.(char-code #\/))))))))
