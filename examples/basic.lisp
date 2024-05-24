(in-package :gficl-examples.basic)

(defparameter *tex* nil)
(defparameter *shader* nil)
(defparameter *quad* nil)
(defparameter *fb* nil)
(defparameter *rb* nil)

(defparameter *view* nil)
(defparameter *projection* nil)

(defun run ()
  (gficl::with-game
   (:title "basic")
   (setup)
   (loop until (gficl:game-closed-p)
	 do (update)
	 do (render))
   (cleanup)))

(defun setup ()
  (setf *tex*
	(gficl::make-texture
	 :rgb 100 100))
  (setf *shader* (gficl::make-shader *vert-shader* *frag-shader*))
  (setf *fb* (gficl::make-framebuffer '((:color-attachment0 :texture)
					(:depth-stencil-attachment :renderbuffer))
				      100 100 1))
  (setf *rb* (gficl::make-renderbuffer :rgb 100 100 1))
  (setf *quad* (gficl::make-vertex-data
		gficl::*3d-vertex*
		'(((0 0 0) (0 0))
		  ((1 0 0) (1 0))
		  ((1 1 0) (1 1))
		  ((0 1 0) (0 1)))
		'(0 3 2 2 1 0)))
  (setf *view* (gficl::make-matrix 4))
  (setf *projection* (gficl::make-matrix 4)))

(defun cleanup ()
  (gficl::delete-gl *tex*)
  (gficl::delete-gl *shader*)
  (gficl::delete-gl *fb*)
  (gficl::delete-gl *rb*)
  (gficl::delete-gl *quad*))

(defun render ()
  (gficl::with-render
   (gl:bind-framebuffer :framebuffer (gficl::id *fb*))
   (gl:bind-framebuffer :framebuffer 0)
   (gl:use-program (gficl::id *shader*))
   (gficl::set-shader-matrix *shader* "view" *view*)
   (gficl::set-shader-matrix *shader* "projection" *projection*)
   (gficl::draw-vertex-data *quad*)))

(defun update ()
  (gficl::with-update
   (destructuring-bind (w h) (glfw:get-window-size)
		       ;;(setf *projection* (gficl::ortho-matrix w h 0 1))
		       )))


(defparameter *vert-shader*
	      "#version 330
layout (location = 0) in vec3 vertex;
//layout (location = 1) in vec3 normal;
layout (location = 1) in vec2 inTexCoords;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 projection;
uniform mat4 view;

void main()
{
    TexCoords = inTexCoords;
//projection * view * model *
    gl_Position = projection * view * vec4(vertex, 1.0);
}")
(defparameter *frag-shader*
"#version 330

in vec2 TexCoords;
out vec4 colour;

void main() {
     colour = vec4(TexCoords.x, TexCoords.y, 0, 1);
}
")
