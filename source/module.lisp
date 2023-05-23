(in-package :chil)

;; NOTE: Wrapping the symbol name in + is not done here (e.g. +ps+) because I
;; think having the bare symbol makes things easier to read.
(defconstant s 's "Seconds as a symbol evaluating to itself.")
(defconstant ms 'ms "Milliseconds as a symbol evaluating to itself.")
(defconstant us 'us "Microseconds as a symbol evaluating to itself.")
(defconstant ns 'ns "Nanoseconds as a symbol evaluating to itself.")
(defconstant ps 'ps "Picoseconds as a symbol evaluating to itself.")
(defconstant fs 'fs "Femtoseconds as a symbol evaluating to itself.")

(defparameter *valid-time-units* (list s ms us ns ps fs))

(defun valid-time-unitp (obj)
  "Verify if provided OBJ uses a valid time unit."
  (member obj *valid-time-units*))

(defclass time-spec ()
  ((amount
    :initarg :amount
    :initform 1
    :accessor amount)
   (units
    :initarg :units
    :initform ps
    :accessor units)))

(defun make-time-spec (amount units)
  "Create a time-spec object with the provided AMOUNT and UNITS."
  (assert (and (integerp amount)
               (not (minusp amount))
               (valid-time-unitp units)))
  (make-instance 'time-spec :amount amount :units units))

(defmethod print-object ((obj time-spec) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((amount amount)
                     (units units))
        obj
      (format stream "Amount: ~a, Units: ~a @" amount units))))

;; TODO: timescale constructor should verify that time-unit and time-precision
;; make sense together. E.G. time-unit = 1ps while time-precision=1ms makes no
;; sense.
(defstruct (timescale
            (:constructor timescale (&optional time-unit time-precision)))
  (time-unit (make-time-spec 1 ns)
   ;; "Measurement of delays and simulation time."
   :type time-spec)
  (time-precision (make-time-spec 1 ps)
   ;; "How delay values are rounded before being used in simulation. Namely, the
   ;; maximal time-resolution for this module."
   :type time-spec))

(defstruct (module
            (:constructor module (name &key (inputs '()) (outputs '()))))
  (name "" :type string)
  ;; FIXME: inputs/outputs should be set, not list. We want to prevent people
  ;; from using the same symbol in the input/output collections.
  ;; TODO: But we DO allow the same object to be present in both.
  (inputs '() :type list)
  (outputs '() :type list))
