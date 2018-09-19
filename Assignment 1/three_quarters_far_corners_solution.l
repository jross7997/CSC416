;;;; File: three_quarters_far_corners_solution.l
;;;; Line: Lisp solution for "Three Quarters / Far Corners" Problem
;;;; Written by Justin Ross for CSC 416

;; observations and variable declarations
;diameter of a quarter in inches
(setf diam-quarter 0.955)
(setf three-quarters (* 3 diam-quarter))

;; related deductions
(setf height-rectangle diam-quarter)
(setf length-rectangle three-quarters)
(setf height-sq (expt height-rectangle 2))
(setf length-sq (expt length-rectangle 2))

;; compute final result
(setf hypotenuse (sqrt (+ height-sq length-sq)))

