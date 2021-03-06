; testing drawing a board with ltk
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

;;;;;;;;;;;;;;;;;; GLOBAL VARIABLES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *S_WIDTH* 640)
(defvar *S_HEIGHT* 480)

(defvar *PIECE_SIZE* 40)

(defvar *playerturn* 1) ; keeps track of whose turn it is
(defvar *isrunning* t) ; t when a win has occured

; the board - 0 means open, 1 means player 1, 2 means player 2
(defparameter *board* '(0 0 0 0 0 0 0
                        0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0
						0 0 0 0 0 0 0))

;(defparameter *player-enter* 0)
(defparameter *player-row* -1)
(defparameter *player-col* -1)

(defparameter *player2-col* -1)

; global variables to keep track of the current 'height' of each column
; shows where to place the piece in terms of height
(defparameter *rowLocs* '(5 5 5 5 5 5 5))

; used in the main loop to take player input
;(defparameter *player-enter* 0)

; seed the lisp random generator
(setf *random-state* (make-random-state t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;; BOARD DEBUG FUNCTIONS;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;; INTERNAL BOARD FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; *row* to be replaced with 'falling' function
; in a single list, row/column is calculated through (row*width + colummn)
; player is either a 1 or 2
; *row* to be replaced with 'falling' function
; in a single list, row/column is calculated through (row*width + colummn)
; player is either a 1 or 2
(defun place-piece(player column canvas)
	(if (equal (nth column *rowLocs*) -1)
		; if the column thing already equals 0
		(format t "Column ~a is already full!~%~%" column)
		; else
		(let (
				(canvas-piece (create-oval canvas (+ (* (+ column 1) 60) 60) ; x1
				                                  (+ (* (nth column *rowLocs*) 60) 60) ; y1
				                                  (+ (+ (* (+ column 1) 60) 60) *PIECE_SIZE*); x2
				                                  (+ (+ (* (nth column *rowLocs*) 60) 60) *PIECE_SIZE*) ; y2
				              )
				)
			)
			;board[7*rowLocs[column]+column] = player;
			;basically board[row][column] = player
			(setf (nth (+ (* 7 (nth column *rowLocs*)) column) *board*) player)
			; rowLocs[column] -= 1
			(setf (nth column *rowLocs*) (- (nth column *rowLocs*) 1))
			
			; draw the piece on the canvas
			(when (equal player 1) 
			    (itemconfigure canvas canvas-piece :fill "black") ; true
			)
			(when (equal player 2)
				(itemconfigure canvas canvas-piece :fill "red")   ; false, so player 2
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		;(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player1-win-vertical) t)
	  (let()
		;(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player1-win-diagonal) t)
	  (let()
		;(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;;;;;;;; test player 2 for wins ;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	(if (eq (test-player2-win-horizontal) t)
	  (let()
		;(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player2-win-vertical) t)
	  (let()
		;(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player2-win-diagonal) t)
	  (let()
		;(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	nil ; return false if no wins
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;; DRAWING FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; function to draw a line
(defun draw-line(canvas coords)
	(let* (
		   (line1 (create-line canvas coords))
		)
	
		(itemconfigure canvas line1 :fill "black")
	)
)

(defvar *lineArgs* '(0 0 0 0))
(defun draw-board(canvas)

	(let (
		  (p1key-circle (create-oval canvas 90 445 110 465))
		  (p2key-circle (create-oval canvas 210 445 230 465))
		 )
	
	; draw the title message on the canvas
	(create-text canvas 280 10 "Connect 4")

	; draw vertical lines
	(loop for x from 1 to 8
		do(
			let(
				(x1 (+ (* x 60) 50))
				(y1 50)
				(x2 (+ (* x 60) 50))
				(y2 410)
			)
				(setf (nth 0 *lineArgs*) x1)
				(setf (nth 1 *lineArgs*) y1)
				(setf (nth 2 *lineArgs*) x2)
				(setf (nth 3 *lineArgs*) y2)
				(draw-line canvas *lineArgs*)
				;(draw-line '(80 20 80 460))
		)
	)

	; draw horizontal lines
	(loop for y from 0 to 6
		do(
			let(
				(x1 110)
				(y1 (+ (* y 60) 50))
				(x2 (- *S_WIDTH* 110))
				(y2 (+ (* y 60) 50))
			)
				(setf (nth 0 *lineArgs*) x1)
				(setf (nth 1 *lineArgs*) y1)
				(setf (nth 2 *lineArgs*) x2)
				(setf (nth 3 *lineArgs*) y2)
				(draw-line canvas *lineArgs*)
				;(draw-line '(80 20 80 460))
		)
	)

		; draw the key
		(create-text canvas 20 450 "Player 1 = ")
		(itemconfigure canvas p1key-circle :fill "black")
		(create-text canvas 140 450 "Player 2 = ")
		(itemconfigure canvas p2key-circle :fill "red")

		; testing the size of an actual piece
		;(create-oval *c* 120 60 (+ 120 *PIECE_SIZE*) (+ 60 *PIECE_SIZE*)) 
	)

)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;; EVENT FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; checks which version of lisp this is and calls the correct exit
(defun my-quit ()
  #+sbcl (sb-ext:quit)
  #+clisp (ext:exit)
  #+ccl (ccl:quit)
  #+allegro (excl:exit))

(defun bind-buttonPress(canvas)
	(bind canvas "<ButtonPress-1>"
		(lambda (evt)
			(if (and (equal *playerturn* 1) (eq *isrunning* t))
				(let ()
					(setf down t)
					(cond 
						;((equal *player-enter* -1) (return 0))
						((and (> (event-x evt) 110) (< (event-x evt) 170)) (let() (setf *playerturn* 2) (place-piece 1 0 canvas)))
						((and (> (event-x evt) 170) (< (event-x evt) 230)) (let() (setf *playerturn* 2) (place-piece 1 1 canvas)))
						((and (> (event-x evt) 230) (< (event-x evt) 290)) (let() (setf *playerturn* 2) (place-piece 1 2 canvas)))
						((and (> (event-x evt) 290) (< (event-x evt) 350)) (let() (setf *playerturn* 2) (place-piece 1 3 canvas)))
						((and (> (event-x evt) 350) (< (event-x evt) 410)) (let() (setf *playerturn* 2) (place-piece 1 4 canvas)))
						((and (> (event-x evt) 410) (< (event-x evt) 470)) (let() (setf *playerturn* 2) (place-piece 1 5 canvas)))
						((and (> (event-x evt) 470) (< (event-x evt) 530)) (let() (setf *playerturn* 2) (place-piece 1 6 canvas)))
			
						(t (format t "Invalid click range!~%"))
					)
				)
				
				;else
				(format t "Sorry, not your turn! ~%")
			)
			(when (check-wins) (setf *isrunning* nil))
			(when (eq *isrunning* t) (player2-turn canvas))
			(when (check-wins) (setf *isrunning* nil))
			(setf *playerturn* 1)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;; AI/AGENT FUNCTIONS ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; look for any winning moves
(defun check-winning-moves(player) ; the player (either 1 or to) to return the winning column to
	
	; test horizontally
	(loop for x from 0 to 3
		do(
			let()
			;(format t "before nth in rowlocs in check winning moves ~%")
			(loop for y from (nth (+ x 3) *rowLocs*) to 5
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
						;(format t "before when 1 in check winning moves ~%")
						;(format t "X = ~a  Y = ~a ~%" x y)
						(when (and (> y -1)
								(equal (nth (+ (* 7 y) (+ x 0)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 1)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 2)) *board*) player)
								(equal (nth (+ x 3) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 3))
								; false
								;(return-from check-winning-moves -1)
						)

						; first loc empty (x 2 2 2)
						;(format t "before when 2 in check winning moves ~%")
						(when (and (> y -1)
								(equal (nth (+ (* 7 y) (+ x 1)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 2)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 3)) *board*) player)
								(equal (nth (+ x 0) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 0))
								; false
								;(return-from check-winning-moves -1)
						)

						; second loc empty (2 x 2 2)
						;(format t "before when 3 in check winning moves ~%")
						(when (and (> y -1) 
								(equal (nth (+ (* 7 y) (+ x 0)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 2)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 3)) *board*) player)
								(equal (nth (+ x 1) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 1))
								; false
								;(return-from check-winning-moves -1)
						)

						; third loc empty (2 2 x 2)
						;(format t "before when 4 in check winning moves ~%")
						(when (and (> y -1)
								(equal (nth (+ (* 7 y) (+ x 0)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 1)) *board*) player)
						        (equal (nth (+ (* 7 y) (+ x 3)) *board*) player)
								(equal (nth (+ x 2) *rowLocs*) y))
					
					        	;true
						        (return-from check-winning-moves (+ x 2))
								; false
								;(return-from check-winning-moves -1)
						)
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
						;(format t "before test vertically and in check-winning-move ~%")
						(when 
							(and (> y -1)
								; the three y positions below our current = 2
								(equal (nth (+ (* 7 (+ y 3)) x) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) x) *board*) player)
								(equal (nth (+ (* 7 (+ y 1)) x) *board*) player)
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
						;(format t "before check diagonally in check-winning-move ~%")
						; three in a row diagonal to the left (2 2 2 x)
						(when 
							(and (> y -1)
								(equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) player)
								(equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) player)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 3) *rowLocs*) (+ y 3))
							)
							(return-from check-winning-moves (+ x 3))
						)

						; three in a row diagonal to the left (x 2 2 2)
						;(format t "in check diagonally in check-winning-move ~%")
						(when 
							(and (> y -1)
								(equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) player)
								(equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) player)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 0) *rowLocs*) (+ y 0))
							)
							(return-from check-winning-moves (+ x 0))
						)

						; three in a row diagonal to the left (2 x 2 2)
						;(format t "in check diagonally in 2 check-winning-move ~%")
						(when 
							(and (> y -1)
								(equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) (+ x 2)) *board*) player)
								(equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) player)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (+ x 1) *rowLocs*) (+ y 1))
							)
							(return-from check-winning-moves (+ x 1))
						)

						; three in a row diagonal to the left (2 2 x 2)
						;(format t "in check diagonally in 3 check-winning-move ~%")
						(when 
							(and (> y -1)
								(equal (nth (+ (* 7 (+ y 0)) (+ x 0)) *board*) player)
								(equal (nth (+ (* 7 (+ y 1)) (+ x 1)) *board*) player)
								(equal (nth (+ (* 7 (+ y 3)) (+ x 3)) *board*) player)
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
						;(format t "in check diagonally in 4 check-winning-move ~%")
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) player)
								(equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) player)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 0) *rowLocs*) (+ y 0))
							)
							(return-from check-winning-moves (- x 0))
						)

						; three in a row diagonal to the left (2 x 2 2)
						;(format t "in check diagonally in 5 check-winning-move ~%")
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) player)
								(equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) player)
								(equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) player)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 2) *rowLocs*) (+ y 2))
							)
							(return-from check-winning-moves (- x 2))
						)

						; three in a row diagonal to the left (2 2 x 2)
						;(format t "in check diagonally in 6 check-winning-move ~%")
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) player)
								(equal (nth (+ (* 7 (+ y 3)) (- x 3)) *board*) player)
								; and the next space down to the right is the next piece
								; to be played in that column
								(equal (nth (- x 1) *rowLocs*) (+ y 1))
							)
							(return-from check-winning-moves (- x 1))
						)

						; three in a row diagonal to the left (x 2 2 2)
						;(format t "in check diagonally in 7 check-winning-move ~%")
						(when 
							(and
								(equal (nth (+ (* 7 (+ y 0)) (- x 0)) *board*) player)
								(equal (nth (+ (* 7 (+ y 1)) (- x 1)) *board*) player)
								(equal (nth (+ (* 7 (+ y 2)) (- x 2)) *board*) player)
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

; try to avoid getting caught with 3 in the middle and a win on each side
(defun check-3-middle()
	(loop for x from 0 to 3
		do(
			loop for y from 0 to 5
				do(
					when
						(and
							; two in a row with a gap on the left side ( x 1 1 x )
							(equal (nth (+ (* 7 (+ y 0)) (+ x 1)) *board*) 1)
							(equal (nth (+ (* 7 (+ y 0)) (+ x 2)) *board*) 1)

							; the current space on the left is open
							(equal (nth x *rowLocs*) y)
							; the current space on the right is open
							(equal (nth (+ x 3) *rowLocs*) y)
						)
						; TODO - determine wether left or right side is better to place
						(return-from check-3-middle x);
					
				)
		)
	)

	; if no danger of 3 in a row, return -1
	-1
)


(defun player2-turn(canvas)

	; check for a winning move; if so take it
	;(format t "before check winning moves in player 2 turn ~%")
	(setf *player2-col* (check-winning-moves 2))
	(if (not (equal *player2-col* -1))
		(let()
			;(format t "before place piece player 2 ~%")
			(place-piece 2 *player2-col* canvas)
			(return-from player2-turn 0)
		)
	)

	; check for a winning move for player 1; if so block it
	;(format t "check winning moves = ~a ~%" (check-winning-moves))
	;(format t "before check winning moves again in player 2 turn ~%")
	(setf *player2-col* (check-winning-moves 1))
	(if (not (equal *player2-col* -1))
		(let()
			;(format t "before place piece again in player 2 turn ~%")
			(place-piece 2 *player2-col* canvas)
			(return-from player2-turn 0)
		)
	)

	; check for the danger of 3 in a row in the middle of the 
	; board for player 1, if so stop it before it happens
	(setf *player2-col* (check-3-middle))
	(if (not (equal *player2-col* -1))
		(let()
			;(format t "before place piece again in player 2 turn ~%")
			(place-piece 2 *player2-col* canvas)
			(return-from player2-turn 0)
		)
	)


	; otherwise just place a piece randomly
	;(format t "before place piece random ~%")
	; TODO - check to see if the random position hasn't been taken yet!!!
	(place-piece 2 (random 7) canvas)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;; MAIN ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun main()
	(with-ltk ()
		(let (
				(c (make-instance 'canvas :width *S_WIDTH* :height *S_HEIGHT*)) ; canvas drawing instance
			)
			
			(pack c)
			(bind-buttonPress c) ; add an button down event listener
			(draw-board c)
			
			(format t "Welcome to Connect 4!~%~%")

		)
	)
	(format t "Good Bye!~%")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		

; call the function
(main)
