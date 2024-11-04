(in-package :chil)

(defclass uint (data)
  ((value
    :accessor value
    :initarg :value
    :initform 0
    :type integer
    :documentation "The value of the bit pattern, as an unsigned integer."))
  (:documentation "Data type representing unsigned integral values."))

(defun uint (val)
  "Create a unsigned integer object (CHIL:UINT) with the specified VAL."
  (let ((width (chil/utils:log2up val)))
    (make-instance 'uint :value val :width width)))

(defmethod print-object ((obj uint) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (with-accessors ((width width)
                     (value value))
        obj
      (format stream "Width: ~A, Value: ~A" width value))))

(defmethod pack ((data uint))
  "Pack the uint object."
  (format t "Should pack ~A~%" data))

(defmethod unpack ((data uint))
  "Unpack the uint object."
  (format t "Should unpack ~A~%" data))
