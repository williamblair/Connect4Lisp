; testing drawing a board with ltk
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

; try to initialize ltk
(start-wish)

; screen width and height
(defvar *S_WIDTH* 640)
(defvar *S_HEIGHT* 480)

(defvar *PIECE_SIZE* 40)

; create the gui???
;(defvar *sc* (make-instance 'scrolled-canvas))
(defvar *c* (make-instance 'canvas :width *S_WIDTH* :height *S_HEIGHT*))
;(defvar *c* (canvas *sc*))
;(defvar *line* (create-line *c* '(100 100 400 50 700 150)))

; pack the widgets into the gui
;(pack *sc* :expand 1 :fill :both)
(pack *c* :expand 1 :fill :both)
;(scrollregion *c* 0 0 800 800)

; function to draw a line
(defun draw-line(coords)
	(let* (
		   (line1 (create-line *c* coords))
		)
	
		(itemconfigure *c* line1 :fill "black")
	)
)

; testing functionality
(defun draw-test()
	(let* ( 
			(line1 (create-line *c* '(100 100 200 200))) ; list - X Y X Y X Y ... representing each point in the line
			(line2 (create-line *c* '(10 100 50 200)))

			; try to draw a circle
			(circle1 (create-oval *c* 300 300 400 400))
		   )
	  (itemconfigure *c* line1 :fill "blue") ; testing attributes 
	  (itemconfigure *c* circle1 :fill "red")
	)
)

;(defvar x 1)
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
		(create-oval *c* 120 60 (+ 120 *PIECE_SIZE*) (+ 60 *PIECE_SIZE*)) 
	)

)



(draw-board)
;(draw-test) ; what happens if i draw it again? - answer - nothing, clears and draws over it

