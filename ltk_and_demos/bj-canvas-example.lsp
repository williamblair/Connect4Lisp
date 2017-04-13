; testing ltk to see if i need to load anything first
; above didn't work - lets try adding the initial load stuff
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

(defun canvas-example()
	(with-ltk ()
		(let* ((sc (make-instance 'scrolled-canvas))
		        (c (canvas sc))
				(line (create-line c '(100 100 400 50 700 150)))
				(polygon (create-polygon c '(50 150 250 160 250 300 50 330)))
				(text (create-text c 260 250 "Canvas Test")))
	(pack sc :expand 1 :fill :both)
	(scrollregion c 0 0 800 800)
)))


; try running it
(canvas-example)

