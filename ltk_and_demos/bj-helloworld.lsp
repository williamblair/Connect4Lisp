; testing ltk to see if i need to load anything first
; above didn't work - lets try adding the initial load stuff
(compile-file "ltk")
(load "ltk")
(in-package :ltk)

(defun hello-1()
	(with-ltk ()
		(let ((b (make-instance 'button
		                        :master nil
								:text "Press me"
								:command (lambda()
										   (format t "Hello World! ~%")))))
		(pack b))))


; try running it
(hello-1)

