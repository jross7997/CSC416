;;File: dice.l
;;Line: CLOS representation of dice

;;imports
(load "lp.l")

;;Die Object Definition
(defclass die()
  ( (num-of-faces :accessor die-num-of-faces
		  :initarg :num-of-faces :initform 6)
    (top-face :accessor die-top-face :initarg :top-face :initform 1)
    (history :accessor die-history :initform () )
    )
  )

;;Display Method
(defmethod display ((d die))
  (format t "[~A,~A]"
	  (die-top-face d) (die-history d)
	  )
  nil
  )

;;To-String
(defmethod to-string ((d die))
  (format nil "Number of faces: ~A ~%[~A,~A]" (die-num-of-faces d)
	  (die-top-face d) (die-history d))
  )

;;Forget History Method
(defmethod forget ((d die))
  (setf (die-history d) () )
  nil
  )

;;Roll the dice!
(defmethod roll ((d die))
  (setf (die-top-face d) (+ (random (die-num-of-faces d)) 1) )
  (setf (die-history d) (snoc (die-top-face d)(die-history d)))
  nil
  )

;;Demo
(defun dice-demo ()
  (write-line "Create a default, 6-faced die")
  (setf six (make-instance 'die))
  (write-line (to-string six))
  (write-line "Create a 4-faced die")
  (setf four (make-instance 'die :num-of-faces 4))
  (write-line (to-string four))
  (write-line "Create a 12-faced die")
  (setf twelve (make-instance 'die :num-of-faces 12))
  (write-line (to-string twelve))
  (write-line "Rolling six-faced die 3 times")
  (roll six)
  (display six)
  (roll six)
  (display six)
  (roll six)
  (display six)
  (write-line "")
  (write-line "Forgetting six history")
  (forget six)
  (display six)
  (write-line "")
  (write-line "Rolling four-faced die 3 times")
  (roll four)
  (display four)
  (roll four)
  (display four)
  (roll four)
  (display four)
  (write-line "")
  (write-line "Forgetting four history")
  (forget four)
  (display four)
  (write-line "")
  (write-line "Rolling twelve-faced die 3 times")
  (roll twelve)
  (display twelve)
  (roll twelve)
  (display twelve)
  (roll twelve)
  (display twelve)
  (write-line "")
  (write-line "Forgetting twelve history")
  (forget twelve)
  (display twelve)
  nil
  )


;;Roll-for-Pair method
(defmethod roll-for-pair ((d1 die)(d2 die))
  (roll d1)
  (roll d2)
  (if (not (eq (die-top-face d1) (die-top-face d2)))
      (roll-for-pair d1 d2)
    nil
    )
  )

;;Roll-for-pair demo
(defun pairs-demo ()
  (write-line "Creating two dice")
  (setf d1 (make-instance 'die))
  (display d1)
  (setf d2 (make-instance 'die))
  (display d2)
  (write-line "")
  (write-line "rolling-for-pairs Twice")
  (dotimes (x 2)
    (roll-for-pair d1 d2)
    (format t "Roll ~A results: " x)
    (display d1)
    (display d2)
    (write-line "")
    )
  )

;;Roll-for-sum method
(defmethod roll-for-sum ((d1 die) (d2 die) (sum integer))
  (roll d1)
  (roll d2)
  (if (not (eq sum (+ (die-top-face d1) (die-top-face d2))) )
      (roll-for-sum d1 d2 sum)
       nil
       )
  )

;;Roll-for-sum demo
(defun sum-demo ()
  (write-line "Find a random sum between 1 and 12")
  (setf sum (+ (random 12) 1))
  (format t "sum: ~A~%" sum)
  (write-line "Create two dice")
  (setf d1 (make-instance 'die))
  (setf d2 (make-instance 'die))
  (display d1)
  (display d2)
  (write-line "")
  (write-line "Roll for sum twice")
  (dotimes (x 2)
    (roll-for-sum d1 d2 sum)
    (format t "Roll ~A results" x)
    (display d1)
    (display d2)
    (write-line "")
    )
  nil
  )

;;roll-for-7-11-double method
(defmethod roll-for-7-11-double ((d1 die) (d2 die))
  (roll d1)
  (roll d2)
  (if
    (or
      (= (+ (die-top-face d1) (die-top-face d2)) 7)
      (= (+ (die-top-face d1) (die-top-face d2)) 11)
      (= (die-top-face d1) (die-top-face d2))
    )
    nil
    (roll-for-7-11-double d1 d2)
  )
)

