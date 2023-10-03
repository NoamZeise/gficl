(in-package :gficl-examples)

(defun example ()
  (gficl::with-game (:title "example")
   (loop until (gficl::game-closed)
	 do (render)
	 do (update))
    (format t "Exited Example~%")))

(defun render ()
  (gficl::with-render))

(defun update ()
  (gficl::with-update))
