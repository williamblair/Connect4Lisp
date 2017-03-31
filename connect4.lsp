; Skeleton for connect 4 lisp
; William (BJ) Blair
; 03/21/17

; the board - 0 means open, 1 means player 1, 2 means player 2
(defparameter *board* '(0 0 0 0 0 0 0
                        0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0))

; edit to print-board - adding loop
(defun print-row(row)
	(loop for y from 0 to 6
		do(
			format t "~a " (nth (+ (* 7 row) y) *board*)
		)
	)
	(write-line " ") ; move to the next row
)

; calls the above function to print each row
(defun print-board()
	(loop for x from 0 to 5
		do(
			print-row x
		)
	)
	(write-line " ")
)

; global variables to keep track of the current 'height' of each column
; shows where to place the piece in terms of height
(defparameter *rowLocs* '(5 5 5 5 5 5 5))

; easier way to print out the above for debugging
(defun print-rowLocs()
	(loop for x in *rowLocs*
		do(
			format t "~a " x
		)
	)
	(format t "~%")
)

; see if player 1 has won
(defun test-player1-win-horizontal()
	; test horizontal wins for player 1
	(let()
    	(loop for x from 0 to 3
			do(
				loop for y from 0 to 5
					do(
						if ( and (equal (nth (+ (* 7 y) (+ x 0)) *board*) 1)
				       		      (equal (nth (+ (* 7 y) (+ x 1)) *board*) 1)
				   	      	      (equal (nth (+ (* 7 y) (+ x 2)) *board*) 1)
				          	      (equal (nth (+ (* 7 y) (+ x 3)) *board*) 1) )
							;(setf result 1)
							(return-from test-player1-win-horizontal t)
					)
			)
	    ) ; end of test horizontal wins for player 1
		(return-from test-player1-win-horizontal nil)
	)
)

; see if player 2 has won
(defun test-player2-win-horizontal()
	; test horizontal wins for player 1
	(let()
    	(loop for x from 0 to 3
			do(
				loop for y from 0 to 5
					do(
						if ( and (equal (nth (+ (* 7 y) (+ x 0)) *board*) 2)
				       		      (equal (nth (+ (* 7 y) (+ x 1)) *board*) 2)
				   	      	      (equal (nth (+ (* 7 y) (+ x 2)) *board*) 2)
				          	      (equal (nth (+ (* 7 y) (+ x 3)) *board*) 2) )
							;(setf result 1)
							(return-from test-player2-win-horizontal t)
					)
			)
	    ) ; end of test horizontal wins for player 2
		(return-from test-player2-win-horizontal nil)
	)
)

; *row* to be replaced with 'falling' function
; in a single list, row/column is calculated through (row*width + colummn)
; player is either a 1 or 2
(defun place-piece(player column)
	(if (equal (nth column *rowLocs*) -1)
		; if the column thing already equals 0
		(format t "Column ~a is already full!~%~%" column)
		; else
		(let ()
			;board[7*rowLocs[column]+column] = player;
			;basically board[row][column] = player
			(setf (nth (+ (* 7 (nth column *rowLocs*)) column) *board*) player)
			; rowLocs[column] -= 1
			(setf (nth column *rowLocs*) (- (nth column *rowLocs*) 1))
		)
	)
)
						

; the main game loop
(format t "Welcome to Connect 4!~%~%")
(defparameter *player-enter* -1)
(loop
	(print-board)
	(format t "Enter a column to enter (or -1 to quit) >")
	(setf *player-enter* (read))
	(cond 
		((equal *player-enter* -1) (return 0))
		((> *player-enter* -1) 
			(if (< *player-enter* 7)
				;(place-piece 1 *player-enter*)
				(place-piece 2 *player-enter*) ; edited to player 2 for win testing
				(format t "Enter a column from 0 to 6 or q to quit!~%")
			)
		)
		(t (format t "Enter a column from 0 to 6 or q to quit!~%"))
	)
	; test player 1 for wins
	(if (eq (test-player1-win-horizontal) t)
	  (let()
		(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	; test player 2 for wins
	(if (eq (test-player2-win-horizontal) t)
	  (let()
		(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	(when (equal *player-enter* -1) (return 0))
)
(format t "Good Bye!~%")

