; Skeleton for connect 4 lisp
; William (BJ) Blair
; 03/21/17

;;;;;;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the board - 0 means open, 1 means player 1, 2 means player 2
(defparameter *board* '(0 0 0 0 0 0 0
                        0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0))

(defparameter *player-enter* 0)
(defparameter *player-row* -1)
(defparameter *player-col* -1)

(defparameter *player2-col* -1)

; global variables to keep track of the current 'height' of each column
; shows where to place the piece in terms of height
(defparameter *rowLocs* '(5 5 5 5 5 5 5))

; used in the main loop to take player input
(defparameter *player-enter* 0)

; seed the lisp random generator
(setf *random-state* (make-random-state t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; PRINTING FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; edit to print-board - adding loop
(defun print-row(row)
 	(format t "~a  " row)
	(loop for y from 0 to 6
		do(
			format t "~a " (nth (+ (* 7 row) y) *board*)
		)
	)
	(write-line " ") ; move to the next row
)

; calls the above function to print each row
(defun print-board()
	(format t "~%   0 1 2 3 4 5 6~%~%")
	(loop for x from 0 to 5
		do(
			print-row x
		)
	)
	(write-line " ")
)


; easier way to print out the above for debugging
(defun print-rowLocs()
	(loop for x in *rowLocs*
		do(
			format t "~a " x
		)
	)
	(format t "~%")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;; TEST WIN FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; see if player 1 has won
(defun test-player1-win-diagonal()
	; test diagonal wins for player 1
	(let()
    	(loop for x from 0 to 3
			do(
				loop for y from 0 to 2
					do(
						if ( and (equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) 1)
				       		      (equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) 1)
				   	      	      (equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) 1)
				          	      (equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) 1) )
							;(setf result 1)
							(return-from test-player1-win-diagonal t)
					)
			)
	    ) ; end of first loop diagonal for player 1
    	(loop for x from 6 downto 3
			do(
				loop for y from 0 to 2
					do(
						if ( and (equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) 1)
				       		      (equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) 1)
				   	      	      (equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) 1)
				          	      (equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) 1) )
							;(setf result 1)
							(return-from test-player1-win-diagonal t)
					)
			)
	    ) ; end of second loop diagonal for player 1

		; if either neither test returned true then return false
		(return-from test-player1-win-diagonal nil)
	)
)

(defun test-player1-win-vertical()
	; test vertical wins for player 1
	(let()
    	(loop for x from 0 to 6
			do(
				loop for y from 0 to 2
					do(
						if ( and (equal (nth (+ (* 7 (+ y 0)) x) *board*) 1)
				       		      (equal (nth (+ (* 7 (+ y 1)) x) *board*) 1)
				   	      	      (equal (nth (+ (* 7 (+ y 2)) x) *board*) 1)
				          	      (equal (nth (+ (* 7 (+ y 3)) x) *board*) 1) )
							;(setf result 1)
							(return-from test-player1-win-vertical t)
					)
			)
	    ) ; end of test horizontal wins for player 1
		(return-from test-player1-win-vertical nil)
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

(defun test-player2-win-vertical()
	; test vertical wins for player 2
	(let()
    	(loop for x from 0 to 6
			do(
				loop for y from 0 to 2
					do(
						if ( and (equal (nth (+ (* 7 (+ y 0)) x) *board*) 2)
				       		      (equal (nth (+ (* 7 (+ y 1)) x) *board*) 2)
				   	      	      (equal (nth (+ (* 7 (+ y 2)) x) *board*) 2)
				          	      (equal (nth (+ (* 7 (+ y 3)) x) *board*) 2) )
							;(setf result 1)
							(return-from test-player2-win-vertical t)
					)
			)
	    ) ; end of test vertical wins for player 2
		(return-from test-player2-win-vertical nil)
	)
)

; see if player 2 has won
(defun test-player2-win-diagonal()
	; test diagonal wins for player 2
	(let()
    	(loop for x from 0 to 3
			do(
				loop for y from 0 to 2
					do(
						if ( and (equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) 2)
				       		      (equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) 2)
				   	      	      (equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) 2)
				          	      (equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) 2) )
							;(setf result 1)
							(return-from test-player2-win-diagonal t)
					)
			)
	    ) ; end of first loop diagonal for player 1
    	(loop for x from 6 downto 3
			do(
				loop for y from 0 to 2
					do(
						if ( and (equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) 2)
				       		      (equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) 2)
				   	      	      (equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) 2)
				          	      (equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) 2) )
							;(setf result 1)
							(return-from test-player2-win-diagonal t)
					)
			)
	    ) ; end of second loop diagonal for player 2

		; if either neither test returned true then return false
		(return-from test-player2-win-diagonal nil)
	)
)

(defun check-wins()
	;;;;;;;;; test player 1 for wins ;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(if (eq (test-player1-win-horizontal) t)
	  (let()
		(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	(if (eq (test-player1-win-vertical) t)
	  (let()
		(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	(if (eq (test-player1-win-diagonal) t)
	  (let()
		(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;; test player 2 for wins ;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(if (eq (test-player2-win-horizontal) t)
	  (let()
		(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	(if (eq (test-player2-win-vertical) t)
	  (let()
		(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	(if (eq (test-player2-win-diagonal) t)
	  (let()
		(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
	  )
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; PIECE PLACING FUNCTIONS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

; for debug purposes only - places a piece anywhere on the board
(defun place-piece-debug(player row column)
	;board[7*rowLocs[column]+column] = player;
	;basically board[row][column] = player
	(setf (nth (+ (* 7 row) column) *board*) player)
	(setf (nth column *rowLocs*) (- (nth column *rowLocs*) 1))
	; rowLocs[column] -= 1
	;(setf (nth column *rowLocs*) (- (nth column *rowLocs*) 1))
)


;;;;;;;;;;;;; AI/AGENT FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; test if player 1 has 3 pieces in a row horizontally
(defun player2-test-3inarow-horizontal()

  	; first test the farthest left horizontally, so
	; you would have to place the piece to the right
	; of the other 3 to block it
	(loop for row from 0 to 5
		do(
		             ; if the first three pieces are all 1s
			if ( and (equal (nth (+ (* 7 row) 0) *board*) 1)
				     (equal (nth (+ (* 7 row) 1) *board*) 1)
					 (equal (nth (+ (* 7 row) 2) *board*) 1)
				
					 ; and if our current position in height
					 ; is that row
					 (equal (nth 3 *rowLocs*) row)
				)
				; return the 3rd column to place a piece in
				; order to block the player
				(setf *player2-col* 3)
		)
	)

	; now test the middle horizontal 3 in a rows, where
	; the ai can place a piece on either the left or the
	; right side
	(loop for col from 1 to 3
		do(
			loop for row from 0 to 5
				do(
					
					if ( and (equal (nth (+ (* 7 row) (+ col 0)) *board*) 1)
					    	 (equal (nth (+ (* 7 row) (+ col 1)) *board*) 1)
							 (equal (nth (+ (* 7 row) (+ col 2)) *board*) 1)
				
							 ; and if our current position in height
							 ; is that row
							 (equal (nth 3 *rowLocs*) row)
					)

					; now figure out to place a piece on
					; the left or the right of the block of 3
					; for now just randomly choose left or right
					; nth random returns either col-1 or col+2 by randomly selecting
					; either the 0th or 1st position in the given list on the farthest
					; right
					(let((twolocs '((- col 1) (+ col 2))))	
						(format t "- col 1 = ~a ~%" (- col 1))
						(format t "+ col 2 = ~a ~%" (+ col 2))
						(setf *player2-col* (nth (random 2) twolocs))
					)
				)
		)
	)

; copied from test-win-horizontal for player 1
;	(let()
;    	(loop for x from 0 to 3
;			do(
;				loop for y from 0 to 5
;					do(
;						if ( and (equal (nth (+ (* 7 y) (+ x 0)) *board*) 2)
;				       		      (equal (nth (+ (* 7 y) (+ x 1)) *board*) 2)
;				   	      	      (equal (nth (+ (* 7 y) (+ x 2)) *board*) 2)
;				          	      (equal (nth (+ (* 7 y) (+ x 3)) *board*) 2) )
;							;(setf result 1)
;							(return-from test-player2-win-horizontal t)
;					)
;			)
;	    ) ; end of test horizontal wins for player 2
;		(return-from test-player2-win-horizontal nil)
;	)

	;(setf *player2-col* -1) ; end of test-3inarow-horizontal
)

; look for any winning moves
(defun check-winning-moves()
	
	; test horizontally
	(loop for x from 0 to 3
		do(
			loop for y from (nth (+ x 3) *rowLocs*) to 5
				do(
					let()
						;(format t "x+0=~a x+1=~a x+2=~a x+3=~a rowLocs[x+3]=~a ~%" 
						;	(nth (+ (* 7 y)(+ x 0)) *board*)
						;	(nth (+ (* 7 y)(+ x 1)) *board*)
						;	(nth (+ (* 7 y)(+ x 2)) *board*)
						;	(nth (+ (* 7 y)(+ x 3)) *board*)
						;	(nth (+ x 3) *rowLocs*)
						;)

						; basic 3 in a row - fourth spot empty (2 2 2 x)
						(when (and (equal (nth (+ (* 7 y) (+ x 0)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 1)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 2)) *board*) 2)
								(equal (nth (+ x 3) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 3))
								; false
								;(return-from check-winning-moves -1)
						)

						; first loc empty (x 2 2 2)
						(when (and (equal (nth (+ (* 7 y) (+ x 1)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 2)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 3)) *board*) 2)
								(equal (nth (+ x 0) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 0))
								; false
								;(return-from check-winning-moves -1)
						)

						; second loc empty (2 x 2 2)
						(when (and (equal (nth (+ (* 7 y) (+ x 0)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 2)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 3)) *board*) 2)
								(equal (nth (+ x 1) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 1))
								; false
								;(return-from check-winning-moves -1)
						)

						; third loc empty (2 2 x 2)
						(when (and (equal (nth (+ (* 7 y) (+ x 0)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 1)) *board*) 2)
						        (equal (nth (+ (* 7 y) (+ x 3)) *board*) 2)
								(equal (nth (+ x 2) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 2))
								; false
								;(return-from check-winning-moves -1)
						)
				)
		)
	)

	; test vertically
	(loop for x from 0 to 6
		do(
			loop for y from 0 to 2
				do(
					let()
						(when 
							(and
								; the three y positions below our current = 2
								(equal (nth (+ (* 7 (+ y 3)) x) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) x) *board*) 2)
								(equal (nth (+ (* 7 (+ y 1)) x) *board*) 2)
								; the current y position is empty
								(equal (nth (+ (* 7 (+ y 0)) x) *board*) 0)
							)
							(return-from check-winning-moves x)
						)
				)
		)
	)

	; check diagonally (left to right downwards)
	(loop for x from 0 to 3
		do(
			loop for y from 0 to 2
				do(
					let()

						; three in a row diagonal to the left (2 2 2 x)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 3) *rowLocs*) (+ y 3))
							)
							(return-from check-winning-moves (+ x 3))
						)

						; three in a row diagonal to the left (x 2 2 2)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 0) *rowLocs*) (+ y 0))
							)
							(return-from check-winning-moves (+ x 0))
						)

						; three in a row diagonal to the left (2 x 2 2)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 1) *rowLocs*) (+ y 1))
							)
							(return-from check-winning-moves (+ x 1))
						)

						; three in a row diagonal to the left (2 2 x 2)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 2) *rowLocs*) (+ y 2))
							)
							(return-from check-winning-moves (+ x 2))
						)
				)
		)
	)

	; check diagonally (right to left downwards)
	(loop for x from 6 downto 3
		do(
			loop for y from 0 to 2
				do(
					let()

						; three in a row diagonal to the left (2 2 2 x)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 0) *rowLocs*) (+ y 0))
							)
							(return-from check-winning-moves (- x 0))
						)

						; three in a row diagonal to the left (2 x 2 2)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 2) *rowLocs*) (+ y 2))
							)
							(return-from check-winning-moves (- x 2))
						)

						; three in a row diagonal to the left (2 2 x 2)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 1) *rowLocs*) (+ y 1))
							)
							(return-from check-winning-moves (- x 1))
						)

						; three in a row diagonal to the left (x 2 2 2)
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) 2)
								(equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) 2)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 3) *rowLocs*) (+ y 3))
							)
							(return-from check-winning-moves (- x 3))
						)
				)
		)
	)
	

	; if no winning moves, return -1
	-1
)


(defun player2-turn()

	; check for a winning move; if so take it
	;(format t "check winning moves = ~a ~%" (check-winning-moves))
	(setf *player2-col* (check-winning-moves))
	(if (not (equal *player2-col* -1))
		(let()
			(place-piece 2 *player2-col*)
			(return-from player2-turn 0)
		)
	)

	; make sure player 1 doesn't have 3 in a row
		;(player2-test-3inarow-horizontal)
		;(if (not (equal *player2-col* -1))
		; 	; true
		;	(let ()
		;		(place-piece 2 *player2-col*)
		;		(return-from player2-turn 0)
		;	)
		;)

		; otherwise just place a piece randomly
		; (place-piece 2 (random 7))
)
						
;;;;;;;;;;;;;;;;; MAIN LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the main game loop
(format t "Welcome to Connect 4!~%~%")
(loop
	(print-board)
	;(format t "Enter a column to enter (or -1 to quit) >")
	(format t "Enter a col to enter (or -1 to quit) >")
	;(setf *player-enter* (read))
	(setf *player-col* (read))
	(format t "Enter a row to enter (or -1 to quit) >")
	(setf *player-row* (read))
	;(cond 
	;	((equal *player-enter* -1) (return 0))
	;	((> *player-enter* -1) 
	;		(if (< *player-enter* 7)
	;			(place-piece 1 *player-enter*)
	;			;(place-piece 2 *player-enter*) ; edited to player 2 for win testing
	;			;(format t "Enter a column from 0 to 6 or q to quit!~%")
	;		)
	;	)
	;	(t (format t "Enter a column from 0 to 6 or q to quit!~%"))
	;)

	;;;;;;;;;;;; DEBUG ;;;;;;;;;;;;;;;;;;;;;;;;;;
	;; place a piece using the debug placement ;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;(place-piece-debug 1 *player-row* *player-col*)
	(place-piece-debug 2 *player-row* *player-col*)

	; checks for horizontal, vertical, and diagonal wins
	; on players 1 and 2
	(check-wins)

	; now its player two's turn - the agent
	(player2-turn)

	; recheck wins to see if player2 had a winning move
	(check-wins)

	; exit if the player enters a -1
	(when (equal *player-enter* -1) (return 0))
)
(format t "Good Bye!~%")

