(in-package :chil)

(defclass data ()
  ((width
    :accessor width
    :initarg :width
    :initform 0
    :type integer
    :documentation "The bit-width of the data."))
  (:default-initargs :width 0)
  (:documentation "The base of the type hierarchy for wire objects/data types.
The data value must be representable as some number (need not be known at Chisel
compile-time) of bits, and must have methods to pack/unpack structured data
to/from bits.

This class is not intended to be directly instantiated!"))

(defmethod print-object ((obj data) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((width width))
        obj
      (format stream "Width: ~a" width))))

(defgeneric pack (data)
  (:documentation "Pack the data object.")
  (:method (data)
    (format t "Should pack ~A~%" data)))

(defgeneric unpack (data)
  (:documentation "Unpack the data object.")
  (:method (data)
    (format t "Should unpack ~A~%" data)))
