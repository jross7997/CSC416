;;;; File: tethered_goat_solution.l
;;;; Line: Lisp solution for the "Tethered Goat" Problem
;;;; Written by Justin Ross for CSC416

;; Observations
(setf length-barn 100)
(setf width-barn 70)
(setf length-rope 107)

;; Function Declarations
; Area of a 1/2 circle
(defun area-half-circle(x)(* (/ pi 2) (expt x 2)))
; Area of 1/4 circle
(defun area-quarter-circle(x)(* (/ pi 4) (expt x 2)))

;; Related Deductions
;Radius Calculations
(setf radius1 length-rope)
(setf radius2 (- length-rope width-barn))
(setf radius3 (- length-rope length-barn))

;Area Calculations
(setf area1 (area-half-circle radius1))
(setf area2 (area-quarter-circle radius1))
(setf area3 (area-quarter-circle radius2))
(setf area4 (area-quarter-circle radius3))

;; Compute Final Result
(setf total-area (+ area1 area2 area3 area4))
