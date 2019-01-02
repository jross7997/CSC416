;;File: accumulator.l
;;Author: Justin Ross

(defclass accumulator ()
  (
   (value :accessor accumulator-value :initarg :value :initform 0)
   )
  )

(defmethod inc ((a accumulator) (n integer))
  (setf (accumulator-value a) (+ (accumulator-value a) n) )
  )

(defmethod reset ((a accumulator))
  (setf (accumulator-value a) 0)
  )

(defmethod display ((a accumulator))
  (format t "~A" (accumulator-value a) )
  nil
  )

(defun accumulator-demo ()
  (write-line "Creating a new accumulator")
  (setf acc (make-instance 'accumulator))
  (display acc)
  (write-line "")
  (write-line "Increment 10 times")
  (dotimes (x 10)
    (inc acc 1)
    (display acc)
    (write-line "")
    )
  nil
  )
		
