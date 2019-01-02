;; File: lp.l
;; Purpose: a collection of recursive list processing methods for CSC416
;; Author: Justin Ross

;Singleton-p returns a predicate indicating if a list contains single atom
(defun singleton-p (lst)
  (cond
   ( (null lst) nil)
   ( (null (cdr lst) ) t)
   ( t nil)
   )
  )
;RAC returns the last element of a list
(defun rac(lst)
  (cond
   ((singleton-p lst) (car lst) )
   (t (rac (cdr lst) ))
  )
  )
;RDC returns a list without the last element of the list
   (defun rdc(lst)
     (cond
      ((singleton-p lst) () )
      (t (cons (car lst) (rdc (cdr lst))))
      )
     )
;SNOC attaches an element to the end of an existing list
   (defun snoc (lo ll)
     (cond
      ((null ll) (list lo))
      (t (cons (car ll) (snoc lo (cdr ll))) )
      )
     )
;PALINDROME-P returns a predicate indicating is a list is a palindrome
   (defun palindrome-p (lst)
     (cond
      ((null lst) t)
      ((singleton-p lst) t)
      ((EQUAL (car lst) (rac lst)) ( palindrome-p (cdr (rdc lst) ) ) )
      )
     )
 ;SELECT returns the element of a list at a specified index
(defun select (index lst)
  (cond
   ((null lst) nil)
   ((= index 0) (car lst))
   (t (select (- index 1) (cdr lst) ) )
   )
  )
;PICK returns a random element of a list
(defun pick (lst)
  (select (random (length lst)) lst)
  )
;SUM returns the sum of a list
(defun sum (lst)
  (cond
   ( (null lst) 0)
   (t (+ (car lst) (sum (cdr lst) ) ) )
   )
  )
;PRODUCT returns the product of a list
(defun product (lst)
  (cond
   ( (null lst) 1)
   (t (* (car lst) (product (cdr lst))))
   )
  )
;;IOTA generates a list of 1 through n
(defun iota (n)
  (cond
   (( = n 0) nil )
   (t (snoc n (iota (- n 1) ) ) )
   )
  )
;;DUPLICATE generates a list of size n populated with lo
(defun duplicate (lo n)
  (cond
   ( (= n 0) nil )
   (t (cons lo (duplicate lo (- n 1) ) ) )
   )
  )
;;FACTORIAL computes the factorial of n
(defun factorial (n)
  (product (iota n) )
  )
;;POWER computes the the power. b^n
(defun power (b n)
  (product (duplicate b n) )
  )
;;FILTER-IN produces a list that satisfies a predicate
(defun filter-in (pred lst)
  (cond
   ((null lst) nil)
   ((eval (snoc (car lst) pred))
    (cons (car lst) (filter-in pred (cdr lst) ) ) )
   (t (filter-in pred (cdr lst) ))
   )
  )
;;FILTER-OUT produces a list that doesn't satisfy a predicate
(defun filter-out (pred lst)
  (cond
   ((null lst) nil)
   (( not (eval (snoc (car lst) pred)))
    (cons (car lst) (filter-out pred (cdr lst) ) ))
   (t (filter-out pred (cdr lst) ) )
   )
  )
;;TAKE-FROM takes all occurences of lo from lst
(defun take-from (lo lst)
  (cond
   ((null lst) () )
   ((equal (car lst) lo) (take-from lo (cdr lst)))
   (t (cons (car lst) (take-from lo (cdr lst))))
   )
  )
;;TAKE-ONE takes one occurence of lo from lst
(defun take-one (lo lst)
  (cond
   ((null lst) () )
   ((equal lo (car lst)) (cdr lst) )
   (t (cons (car lst) (take-one lo (cdr lst))))
   )
  )
;;RANDOM-PERMITATIONS rearranges a list randomly
(defun random-permutation (lst &aux element remainder)
  (cond
   ((null lst) () )
   (t (setf element (pick lst))
      (setf remainder (take-one element lst))
      (cons element (random-permutation remainder)))
   )
  )
   
