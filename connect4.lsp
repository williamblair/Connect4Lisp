; testing to see how much I can do with my
; current knowledge for AI
; 03/21/17

; the board - 0 means open, 1 means player 1, 2 means player 2
(defparameter *board* '(0 0 0 0 0 0 0
                        0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0)) ; maybe one extra will make it not nil ... 

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
						
; see what it looks like printed out			
;(print-board)
;(format t "~%")
;(place-piece 1 0)

;(print-board)
;(format t "~%")

;(place-piece 2 0)
;(print-rowLocs)

;(print-board)

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
				(place-piece 1 *player-enter*)
				(format t "Enter a column from 0 to 6 or q to quit!~%")
			)
		)
		(t (format t "Enter a column from 0 to 6 or q to quit!~%"))
	)
	(when (equal *player-enter* -1) (return 0))
)
(format t "Good Bye!~%")

