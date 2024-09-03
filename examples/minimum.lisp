(in-package :gficl-examples/minimum)

(defparameter *vert* "
#version 330
layout (location = 0) in vec2 vertex;

void main() {
  gl_Position = vec4(vertex, 0, 1);
}")
(defparameter *frag* "
#version 330
out vec4 colour;

void main() {
  colour = vec4(gl_FragCoord.x / 500, gl_FragCoord.y / 300, 1, 1);
}")

(defun run ()
  (gficl:with-window (:title "minimum")
    (let ((shader (gficl:make-shader *vert* *frag*))
	  (data (gficl:make-vertex-data
		 (gficl:make-vertex-form (list (gficl:make-vertex-slot 2 :float)))
		 '(((0 0.9)) ((-0.9 -0.9)) ((0.9 -0.9))))))
      (gficl:bind-gl shader)
      (loop until (gficl:closedp)
	    do (gficl:with-render ()
		 (gl:clear :color-buffer)
		 (gficl:draw-vertex-data data))
	    do (gficl:with-update ()
		 (gficl:map-keys-pressed (:escape (glfw:set-window-should-close)))))
      (gficl:delete-gl shader)
      (gficl:delete-gl data))))
