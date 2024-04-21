(in-package :gficl-examples)

(defparameter *tex* nil)

(defun example ()
  (gficl::with-game
   (:title "example")
   (setup)
   (loop until (gficl:game-closed-p)
	 do (render)
	 do (update))
   (cleanup)
   (format t "Exited Example~%")))

(defun setup ()
  (setf *tex*
	(gficl::make-texture
	 :rgb 100 100)))

(defun cleanup ()
  (gficl::delete-gl *tex*))

(defun render ()
  (gficl::with-render
   (gl:color 1 1 1)
   (gl:with-primitive
    :polygon
    (gl:vertex 0.1 0.1 0)
    (gl:vertex 0.9 0.1 0)
    (gl:vertex 0.9 0.9 0)
    (gl:vertex 0.1 0.9 0))))

(defun update ()
  (gficl::with-update))
