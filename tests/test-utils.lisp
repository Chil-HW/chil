(defmacro assert-error (error form)
  "Return 't if FORM raises ERROR, else return NIL."
  `(handler-case ,form
     (,error (exp) (declare (ignore exp)) 't)
     (:no-error (exp) (declare (ignore exp)) 'nil)))
