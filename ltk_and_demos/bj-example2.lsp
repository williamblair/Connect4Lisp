; testing ltk to see if i need to load anything first
; above didn't work - lets try adding the initial load stuff
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

(defun hello-1()
	(with-ltk ()
		(let* ((f (make-instance 'frame))
		        (b (make-instance 'button
		           :master f
				   :text "Button 1"
				   :command (lambda()
							(format t "Button 1 Pressed! ~%"))))
				(b2 (make-instance 'button
				    :master f
					:text "Button 2"
				:command (lambda () (format t "Button 2 Pressed!~%")))))
		; other configuration options include :expand t - the widget expands
		; must be sent to the pack function
		(pack f :expand t :fill :both)
		(pack b :side :left :expand t :fill :both) ; other possibles include right, top, and bottom
		(pack b2 :side :left)
		(configure f :borderwidth 3)
		(configure f :relief :raised)
		)))


; try running it
(hello-1)

