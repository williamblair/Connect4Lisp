; testing drawing a board with ltk
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

; try to initialize ltk
(start-wish)

; create the gui???
;(defvar *sc* (make-instance 'scrolled-canvas))
(defvar *c* (make-instance 'canvas :width 640 :height 480))
;(defvar *c* (canvas *sc*))
;(defvar *line* (create-line *c* '(100 100 400 50 700 150)))

; pack the widgets into the gui
;(pack *sc* :expand 1 :fill :both)
(pack *c* :expand 1 :fill :both)
;(scrollregion *c* 0 0 800 800)

; function to draw the board
(defun draw-board()
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

(draw-board)
(draw-board) ; what happens if i draw it again? - answer - nothing, clears and draws over it
