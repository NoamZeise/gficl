(in-package :gficl-examples.basic)

(defparameter *tex* nil)
(defparameter *shader* nil)
(defparameter *quad* nil)

(defun run ()
  (gficl::with-game
   (:title "basic")
   (setup)
   (loop until (gficl:game-closed-p)
	 do (render)
	 do (update))
   (cleanup)))

(defun setup ()
  (setf *tex*
	(gficl::make-texture
	 :rgb 100 100))
  (setf *shader* (gficl::make-shader *vert-shader* *frag-shader*))
  (setf *quad* (gficl::make-vertex-data
		gficl::*3d-vertex*
		'((0 0 0 0 0) (1 0 0 1 0) (1 1 0 1 1) (0 1 0 0 1))
		'(0 3 2 2 1 0))))

(defun cleanup ()
  (gficl::delete-gl *tex*)
  (gficl::delete-gl *shader*)
  (gficl::delete-gl *quad*))

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


(defparameter *vert-shader*
	      "#version 330
layout (location = 0) in vec3 vertex;
layout (location = 1) in vec3 normal;
layout (location = 2) in vec2 inTexCoords;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 projection;
uniform mat4 view;

void main()
{
    TexCoords = inTexCoords;
    gl_Position = projection * view * model * vec4(vertex, 1.0);
}")
(defparameter *frag-shader*
"#version 330

in vec2 TexCoords;
out vec4 colour;

void main() {
     colour = vec4(TexCoords.x, TexCoords.y, 0, 1);
}
")
