(in-package :gficl-examples.basic)

;; opengl objects
(defparameter *tex* nil)
(defparameter *shader* nil)
(defparameter *quad* nil)
(defparameter *fb* nil)
(defparameter *rb* nil)

;; shader input
(defparameter *vertex-format*
   (gficl:make-vertex-form
    (list (gficl:make-vertex-slot 3 :float)
	  (gficl:make-vertex-slot 2 :float))))

;; shader data
(defparameter *view* nil)
(defparameter *projection* nil)

;; shaders
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

uniform sampler2D tex;

void main() {
     colour = texture(tex, TexCoords);
}
")

;; object data
(defparameter *model* nil)
(defparameter *rot* nil)

(defun setup ()
  (setf *tex*
	(gficl::make-texture-with-fn 10 10
	   #'(lambda (x y)
	       (list (floor (* 255 (/ x 10)))
		     (floor (* 255 (/ y 10))) 255 255))))
  
  (setf *shader* (gficl:make-shader *vert-shader* *frag-shader*))

  (setf *fb* (gficl::make-framebuffer
	      '((:color-attachment0 :texture)
		(:depth-stencil-attachment :renderbuffer))
	      100 100 1))

  (setf *rb* (gficl::make-renderbuffer :rgb 100 100 1))

  (setf *quad* (gficl::make-vertex-data
		*vertex-format*
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
  (gficl:delete-gl *tex*)
  (gficl:delete-gl *shader*)
  (gficl:delete-gl *fb*)
  (gficl:delete-gl *rb*)
  (gficl:delete-gl *quad*))


(defun run ()
  (gficl:with-window (:title "basic" :width 1000 :height 1000)
    (setup)
    (loop until (gficl:closed-p)
	  do (update)
	  do (render))
    (cleanup)))


(defun render ()
  (gficl:with-render
   (gl:bind-framebuffer :framebuffer (gficl::id *fb*))
   (gl:bind-framebuffer :framebuffer 0)
   (gl:use-program (gficl::id *shader*))
   (gficl:set-shader-matrix *shader* "view" *view*)
   (gficl:set-shader-matrix *shader* "projection" *projection*)
   (gficl:set-shader-matrix *shader* "model" *model*)
   (gl:active-texture :texture0)
   (gficl::bind-texture *tex*)   
   (gficl:draw-vertex-data *quad*)))

(defun update ()
  (gficl:with-update (dt)
   (setf *rot* (+ *rot* (* dt 1)))
   ;(if (> dt 0) (format t "fps: ~a~%" (round (/ 1 dt))))
   (destructuring-bind (w h) (glfw:get-window-size)
		       (setf *projection* (gficl:screen-ortho-matrix w h)))
   (setf *model* (gficl:*-mat
		  (gficl:translation-matrix 200 200 0)
		  (gficl:translation-matrix 250 250 0)
		  (gficl:2d-rotation-matrix *rot*)
		  (gficl:translation-matrix -250 -250 0)
		  (gficl:scale-matrix 500 500 1)))))
