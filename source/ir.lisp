(defpackage :chil/ir
  (:use :cl)
  (:export #:chil-symbol-regexp))

(in-package :chil/ir)

(alexandria:define-constant chil-symbol-regexp
  '(:greedy-repetition 1 nil
    (:char-class
     ;; [[:alnum:]]
     (:range #\a #\z)
     (:range #\A #\Z)
     (:range #\0 #\9)
     :digit-class
     ;; The symbols
     #\# #\$ #\% #\&
     #\< #\= #\> #\? #\@
     #\^ #\_
     #\- #\/))
  :test #'equal
  :documentation
  "The cl-ppcre \"parse-tree\" for Chil's notion of symbols.

Chil follows in Lisp's footsteps and allows any alphanumeric character, hyphen,
underscore, question mark, exclamation (bang) and angle-brackets to be used in a
symbol.

A Chil module's symbols may be modified when converting to a backend
representation. For example, Verilog disallows hyphens, so they are replaced by
some other valid character.")
