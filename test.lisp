(in-package :gficl)

(defun test-update ()
  (with-update))

(defun test-render ()
  (with-render
    (gl:color 1 1 1)
    (gl:with-primitive :polygon
      (gl:vertex 0.25 0.25 0)
      (gl:vertex 0.75 0.25 0)
      (gl:vertex 0.75 0.75 0)
      (gl:vertex 0.25 0.75 0))))

(with-game (:title "Test")
  ;; test resource creation
  (delete-gl (make-shader #p"shaders/vert.vs" #p"shaders/frag.fs"))
  (delete-gl (make-texture :rgba 100 100))
  (delete-gl (make-renderbuffer :rgb 100 100 1))
  (let ((fb (create-framebuffer
	     '((:color-attachment0 :texture)
	       (:depth-stencil-attachment :renderbuffer))
	     100 100 1)))
    (gl:bind-framebuffer :draw-framebuffer (framebuffer-attach-id fb 0))
    (gl:bind-framebuffer :draw-framebuffer 0)
    (delete-gl fb))
  (loop until (gficl::game-closed-p)
	do (test-render)
	do (test-update))
  (format t "Exited Example~%"))
