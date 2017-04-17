; Skeleton for connect 4 lisp
; William (BJ) Blair
; 03/21/17

;;;;;;;;;;;;;;;;;;;; GUI STUFF ;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; initialize gui
; testing drawing a board with ltk
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

; try to initialize ltk
;(start-wish)

; screen width and height
(defvar *S_WIDTH* 640)
(defvar *S_HEIGHT* 480)

(defvar *PIECE_SIZE* 40)

; create the gui???
;(defvar *sc* (make-instance 'scrolled-canvas))
;(defvar *c* (make-instance 'canvas :width *S_WIDTH* :height *S_HEIGHT*))
(defvar *c* nil)
;(defvar *c* (canvas *sc*))
;(defvar *line* (create-line *c* '(100 100 400 50 700 150)))

; pack the widgets into the gui
;(pack *sc* :expand 1 :fill :both)
;(pack *c* :expand 1 :fill :both)
;(scrollregion *c* 0 0 800 800)

; function to draw a line
(defun draw-line(coords)
	(let* (
		   (line1 (create-line *c* coords))
		)
	
		(itemconfigure *c* line1 :fill "black")
	)
)

(defvar *lineArgs* '(0 0 0 0))
(defun draw-board()

	(let (
		  (p1key-circle (create-oval *c* 90 445 110 465))
		  (p2key-circle (create-oval *c* 210 445 230 465))
		 )
	
	; draw the title message on the canvas
	(create-text *c* 280 10 "Connect 4")

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
				(draw-line *lineArgs*)
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
				(draw-line *lineArgs*)
				;(draw-line '(80 20 80 460))
		)
	)

		; draw the key
		(create-text *c* 20 450 "Player 1 = ")
		(itemconfigure *c* p1key-circle :fill "black")
		(create-text *c* 140 450 "Player 2 = ")
		(itemconfigure *c* p2key-circle :fill "red")

		; testing the size of an actual piece
		;(create-oval *c* 120 60 (+ 120 *PIECE_SIZE*) (+ 60 *PIECE_SIZE*)) 
	)

)

; draw the first instance of the board
(draw-board)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player1-win-vertical) t)
	  (let()
		(print-board)
	  	(format t "Player 1 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player1-win-diagonal) t)
	  (let()
		(print-board)
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
		(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player2-win-vertical) t)
	  (let()
		(print-board)
	  	(format t "Player 2 Wins!~%")
		(setf *player-enter* -1) ; make the game exit
		(return-from check-wins t) ; exit this loop
	  )
	)
	(if (eq (test-player2-win-diagonal) t)
	  (let()
		(print-board)
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

;;;;;;;;;;;;;; PIECE PLACING FUNCTIONS ;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; edit - trying to add drawing a gui

; *row* to be replaced with 'falling' function
; in a single list, row/column is calculated through (row*width + colummn)
; player is either a 1 or 2
(defun place-piece(player column)
	(if (equal (nth column *rowLocs*) -1)
		; if the column thing already equals 0
		(format t "Column ~a is already full!~%~%" column)
		; else
		(let (
				(canvas-piece (create-oval *c* (+ (* (+ column 1) 60) 60) ; x1
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
			    (itemconfigure *c* canvas-piece :fill "black") ; true
			)
			(when (equal player 2)
				(itemconfigure *c* canvas-piece :fill "red")   ; false, so player 2
			)
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
	(setf (nth column *rowLocs*) (- row 1))
)


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


(defun player2-turn()

	; check for a winning move; if so take it
	;(format t "before check winning moves in player 2 turn ~%")
	(setf *player2-col* (check-winning-moves 2))
	(if (not (equal *player2-col* -1))
		(let()
			;(format t "before place piece player 2 ~%")
			(place-piece 2 *player2-col*)
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
			(place-piece 2 *player2-col*)
			(return-from player2-turn 0)
		)
	)

	; check for the danger of 3 in a row in the middle of the 
	; board for player 1, if so stop it before it happens
	(setf *player2-col* (check-3-middle))
	(if (not (equal *player2-col* -1))
		(let()
			;(format t "before place piece again in player 2 turn ~%")
			(place-piece 2 *player2-col*)
			(return-from player2-turn 0)
		)
	)


	; otherwise just place a piece randomly
	;(format t "before place piece random ~%")
	; TODO - check to see if the random position hasn't been taken yet!!!
	(place-piece 2 (random 7))
)
						
;;;;;;;;;;;;;;;;; MAIN LOOP ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; the main game loop
(defun main()
	(with-ltk()
		(let ((*c* (make-instance 'canvas :width *S_WIDTH* :height *S_HEIGHT*)))
			;(setf *c* (make-instance 'canvas :width *S_WIDTH* :height *S_HEIGHT*))
			(pack *c* :expand 1 :fill :both)
			;(format t "Welcome to Connect 4!~%~%")
			;(loop
			;	(print-board)
			;	(format t "Enter a column to enter (or -1 to quit) >")
			;	;(format t "Enter a col to enter (or -1 to quit) >")
			;	(setf *player-enter* (read))
			;	;(setf *player-col* (read))
			;	;(format t "Enter a row to enter (or -1 to quit) >")
			;	;(setf *player-row* (read))
			;	;(format t "before cond in main~%")
			;	(cond 
			;		((equal *player-enter* -1) (return 0))
			;		((> *player-enter* -1) 
			;			(if (< *player-enter* 7)
			;				(let()
			;					;(format t "before place piece in main~%")
			;					(place-piece 1 *player-enter*)
			;				)
			;				;(place-piece 2 *player-enter*) ; edited to player 2 for win testing
			;				;(format t "Enter a column from 0 to 6 or q to quit!~%")
			;			)
			;		)
			;		(t (format t "Enter a column from 0 to 6 or q to quit!~%"))
			;	)

				;;;;;;;;;;;; DEBUG ;;;;;;;;;;;;;;;;;;;;;;;;;;
				;; place a piece using the debug placement ;;
				;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
				;(place-piece-debug 1 *player-row* *player-col*)
				;(place-piece-debug 2 *player-row* *player-col*)

				; checks for horizontal, vertical, and diagonal wins
				; on players 1 and 2
				;(format t "before check wins 1 in main~%")
			;	(when (check-wins) (return 0))

				; now its player two's turn - the agent
				;(format t "before player 2 turn in main~%")
			;	(player2-turn)

				; recheck wins to see if player2 had a winning move
				;(format t "before check wins 2 in main~%")
			;	(when (check-wins) (return 0))

				; exit if the player enters a -1
			;	(when (equal *player-enter* -1) (return 0))
			;)
			;(format t "Good Bye!~%")

			; closes the gui
			;(stop-wish)
			;(do-execute "pkill" '("wish"))
		)
	)
)

; run main
(main)

