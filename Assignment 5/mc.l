;;File name: mc.l
;;Missionaries and Cannibals Problem
;;Interactive Framework

(load "lp.l")

( defun mc ()
	( establish-world )
	( init-move-list )
	( make-moves )
)

( defun make-moves ()
	( display-world ) 
	( cond
		( ( goalp )
			( write-line "good work!" )
			nil
		)
		( ( feast-state-p )
			( write-line "Yummy yummy yummy, I got Good in my tummy!!" )
			nil
		)
		( t
			( let ( ( m ( read ) ) )
				( if ( applicable-p m )
					( let () ( perform-move m ) ( make-moves ) )
					( let () ( write-line "move inapplicable" ) nil )
				)
			)
		)
	)
)

(defun perform-move (move)
  (setf *move-list* (snoc move *move-list*))
  (if (equal (current-bank) *left-bank*)
      (move-lr move)
    (move-rl move)
    )
  )

(defun move-lr (ml)
  (if (null ml)(return-from move-lr) )
  (move-lr-1 (first ml) )
  (move-lr (rest ml))
  )

(defun move-rl (ml)
  (if (null ml) (return-from move-rl) )
  (move-rl-1 (first ml) )
  (move-rl (rest ml) )
  )
  
 ;Establishes the world by setting the left and right banks to the initial states 
(defun establish-world()
  (setf *left-bank* '(M M M C C C B) )
  (setf *right-bank* () )
  )

 ;Inititalize move list 
(defun init-move-list ()
  (setf *move-list* ())
  )

;display the current status
(defun display-world ()
  (format t "*left-bank* ~S~%" *left-bank*)
  (format t "*right-bank* ~S~%" *right-bank*)
  )

;define a goal state  
(defun goalp ()
  (sort *right-bank* #'string< :key #'symbol-name)
  (cond
   ( (equal *right-bank* '(B C C C M M M) )
     t
     )
   ( (equal *right-bank* '(C C C M M M) )
     t
     )
   (t
    nil
    )
   )
  )

;Check if the cannibals can feast  
(defun feast-state-p ()
  (setf left-can-ct (count 'C *left-bank*) )
  (setf left-mis-ct (count 'M *left-bank*) )
  (setf right-can-ct (count 'C *right-bank*) )
  (setf right-mis-ct (count 'M *right-bank*))
  (cond
   ((and (> left-can-ct left-mis-ct) (> left-mis-ct 0) )
    t
    )
   ((and (> right-can-ct right-mis-ct) (> right-mis-ct 0) )
    t
    )
   (t
    nil
    )
   )
  )

;determine if a move is applicable.   
(defun applicable-p (move)
  (setf boat-p (equal 'B (find 'B move)) )
  (setf left-boat-p (equal 'B (find 'B *left-bank*)))
  (setf right-boat-p (equal 'B (find 'B *right-bank*)))
  (setf move-can-ct (count 'C move))
  (setf move-mis-ct (count 'M move))
  (setf left-can-ct (count 'C *left-bank*))
  (setf left-mis-ct (count 'M *left-bank*))
  (setf right-can-ct (count 'C *right-bank*))
  (setf right-mis-ct (count 'M *right-bank*))
  (cond
   (boat-p
    (cond
     (left-boat-p 
       (And (>= left-can-ct move-can-ct) (>= left-mis-ct move-mis-ct))
      )
      (right-boat-p 
       (and (>= right-can-ct move-can-ct) (>= right-mis-ct move-mis-ct))
      )
     (t
      nil
      )
     )
    )
   ( t
     nil
     )
   )
  )

;determine which bank the boat is on
( defun current-bank ()
	( cond
		( ( > (count 'b *left-bank* ) 0 )
			*left-bank*
		)
		( t
			*right-bank*
		)
	)
)

;move from the left bank to the right bank
(defun move-lr-1 (move)
  (setf *right-bank* (cons move *right-bank*))
  (setf *left-bank* (remove move *left-bank* :count 1 ))
  )

;Move from the right bank to the left bank
(defun move-rl-1 (move)
  (setf *left-bank* (cons move *left-bank*))
  (setf *right-bank* (remove move *right-bank* :count 1))
  )

;Displays the previous move list
(defun display-solution ()
	(mapcar #'print *move-list*)
	nil
  )
