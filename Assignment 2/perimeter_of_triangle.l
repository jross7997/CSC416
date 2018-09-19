; File: perimeter_of_triangle.l
; Line: Lisp program to find the perimeter of a triangle
; Written for CSC 416
; Author: Justin Ross

(defun distance (x1 y1 x2 y2)
  (setf result (sqrt (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2) ) ) ) 
  (return-from distance result)
)

(defun perimeter (x1 y1 x2 y2 x3 y3)
  (setf side1 (distance x1 y1 x2 y2 ) )
  (setf side2 (distance x1 y1 x3 y3 ) )
  (setf side3 (distance x2 y2 x3 y3 ) )
  (setf result (+ side1 side2 side3) )
  (return-from perimeter result)
  )
  




