(in-package :gficl-examples.basic)

;; opengl objects
(defparameter *tex* nil)
(defparameter *shader* nil)
(defparameter *quad* nil)
(defparameter *fb* nil)

;; shader input
(defparameter *vertex-format*
   (gficl:make-vertex-form
    (list (gficl:make-vertex-slot 3 :float)
	  (gficl:make-vertex-slot 2 :float))))

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
}")

;; shader data
(defparameter *view* nil)
(defparameter *projection* nil)

;; object data
(defparameter *model* nil)
(defparameter *rot* nil)

(defparameter *samples* 1)

(defun setup ()
  (setf *shader* (gficl:make-shader *vert-shader* *frag-shader*))
  (gl:clear-color 0 0 0 0)
  (setf *samples* (min 16 (gl:get-integer :max-samples)))
  (if (> *samples* 1) (gl:enable :multisample))  
  (setf *fb* nil)
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test)
  (setf *quad* (gficl:make-vertex-data
		*vertex-format*
		'(((0 0 0) (0 0))
		  ((1 0 0) (1 0))
		  ((1 1 0) (1 1))
		  ((0 1 0) (0 1)))
		'(0 3 2 2 1 0)))
  (setf *tex* (gficl:make-texture-with-fn
	       10 10 #'(lambda (x y) (list (floor (* 255 (/ x 10)))
					   (floor (* 255 (/ y 10)))
					   255 255))))
  
  (setf *view* (gficl:make-matrix 4))
  (setf *projection* (gficl:make-matrix 4))

  (setf *model* (gficl:make-matrix 4))
  (setf *rot* 0))

(defun resize (w h)
  (if *fb* (gficl:delete-gl *fb*))
  (setf *fb* (gficl:make-framebuffer
	      (list (gficl:make-attachment-description :color-attachment0)
		    (gficl:make-attachment-description :depth-stencil-attachment))
	      w h *samples*)))

(defun cleanup ()
  (gficl:delete-gl *tex*)
  (gficl:delete-gl *shader*)
  (if *fb* (gficl:delete-gl *fb*))
  (gficl:delete-gl *quad*))

(defun run ()
  (gficl:with-window
   (:title "basic" :width 1000 :height 1000 :resize-callback #'resize)
   (setup)
    (loop until (gficl:closed-p)
	  do (update)
	  do (render))
    (cleanup)))

(defun render ()
  (gficl:with-render
   
   (gficl:bind-gl *fb*)
   (gl:viewport 0 0 (gficl::window-width) (gficl::window-height))
   (gl:clear :color-buffer :depth-buffer)
   
   (gficl:bind-gl *shader*)
   (gficl:bind-matrix *shader* "view" *view*)
   (gficl:bind-matrix *shader* "projection" *projection*)
   (gficl:bind-matrix *shader* "model" *model*)
   (gl:active-texture :texture0)
   (gficl:bind-gl *tex*)
   (gficl:draw-vertex-data *quad*)
   (gficl:bind-matrix *shader* "model"
     (gficl:*-mat
      (gficl:translation-matrix 200 200 0.1)
      (gficl:scale-matrix 500 500 1)))
   (gficl:draw-vertex-data *quad*)

   (gficl:blit-framebuffers *fb* 0 (gficl:window-width) (gficl:window-height))))

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
