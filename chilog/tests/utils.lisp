(defpackage :chilog/tests/utils
  (:use :cl)
  (:export #:assert-set-equal
           #:assert-set-not-equal
           #:complex-generator))

(in-package :chilog/tests/utils)

(defmacro assert-set-equal (&whole whole expected form
                            &rest extras
                            &key (test #'equal))
  "Assert whether expected and form are equal as sets, using the equality test
provided in :TEST. By default :TEST is set to `equal'."
  ;; XXX: lisp-unit2 does not export these 2 symbols, so we have rely on them
  ;; having this name!
  `(lisp-unit2::expand-assert
    'lisp-unit2::equal-result ,form ,form ,expected ,extras
    :test #'(lambda (s1 s2) (not (set-exclusive-or s1 s2 :test ,test)))
    :full-form ',whole))

(defmacro assert-set-not-equal (&whole whole expected form
                                &rest extras
                                &key (test #'equal))
  "Assert whether expected and form are not equal as sets, using the equality
test provided in :TEST. By default :TEST is set to `equal'."
  ;; XXX: lisp-unit2 does not export these 2 symbols, so we have rely on them
  ;; having this name!
  `(lisp-unit2::expand-assert
    'lisp-unit2::equal-result ,form ,form ,expected ,extras
    :test #'(lambda (s1 s2) (set-exclusive-or s1 s2 :test ,test))
    :full-form ',whole))

(check-it:def-generator complex-generator (real-gen imag-gen)
  ;; Call the two generators provided to the complex-generator and pass those
  ;; values to #'complex to build a complex number, returning that.
  (check-it:generator
   (complex (check-it:generate real-gen)
            (check-it:generate imag-gen))))
