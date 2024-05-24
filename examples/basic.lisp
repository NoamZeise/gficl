(in-package :gficl-examples.basic)


;; glfw objects
(defparameter *tex* nil)
(defparameter *shader* nil)
(defparameter *quad* nil)
(defparameter *fb* nil)
(defparameter *rb* nil)

;; shader data
(defparameter *view* nil)
(defparameter *projection* nil)

;; object data
(defparameter *model* nil)
(defparameter *rot* nil)

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
  (setf *projection* (gficl::make-matrix 4))

  (setf *model* (gficl::make-matrix 4))
  (setf *rot* 0))

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
   (gficl::set-shader-matrix *shader* "model" *model*)
   (gficl::draw-vertex-data *quad*)))

(defparameter *last-stamp* 0)
(defparameter *this-stamp* 0)
(defparameter *dt* 1)

(defun update ()
  (gficl::with-update
   (setf *this-stamp* (get-internal-real-time))
   (setf *dt* (/ (- *this-stamp* *last-stamp*)
		 internal-time-units-per-second))
   (setf *rot* (+ *rot* (* *dt* 1)))
   (destructuring-bind (w h) (glfw:get-window-size)
		       (setf *projection* (gficl::screen-ortho-matrix w h)))
   (setf *model* (gficl::*-mat		  
		  (gficl::translation-matrix 150 100 0)
		  (gficl::2d-rotation-matrix *rot*)
		  (gficl::scale-matrix 100 50 1))))
  (setf *last-stamp* *this-stamp*))


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
