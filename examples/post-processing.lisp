(in-package :gficl-examples.post-processing)

(defparameter *main-vert*
  "#version 330
layout (location = 0) in vec3 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
  gl_Position = projection * view * model * vec4(position, 1);
}")

(defparameter *main-frag*
  "#version 330

out  vec4 colour;

void main() {
   colour = vec4(1);
}")

(defparameter *post-vert*
  "#version 330
out vec2 uv;
uniform mat4 transform;
void main() {
  uv = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
  gl_Position = transform * vec4(uv * 2.0f - 1.0f, 0.0f, 1.0f);
}")

(defparameter *post-frag*
  "#version 330
in vec2 uv;
out vec4 colour;
uniform sampler2D screen;
void main() { colour = texture(screen, uv); }")

(defparameter *cube-data*
  (list :verts
	'(((-1 -1 -1))
	  ((-1 -1  1))
	  ((-1  1 -1))
	  ((-1  1  1))
	  (( 1 -1 -1))
	  (( 1 -1  1))
	  (( 1  1 -1))
	  (( 1  1  1)))
	:indices
	'(2 1 0 1 2 3
	  4 5 6 7 6 5
	  0 1 4 5 4 1
	  6 3 2 3 6 7
	  4 2 0 2 4 6
	  1 3 5 7 5 3)))

(defparameter *samples* nil)

;; normal render
(defparameter *main-shader* nil)
(defparameter *offscreen-fb* nil)

;; post-processing 
(defparameter *dummy-vert* nil)
(defparameter *post-shader* nil)

(defparameter *target-width* 1000)
(defparameter *target-height* 700)

;; scene
(defparameter *cube* nil)

(defun setup ()
  ;(setf *samples* (min 8 (gl:get-integer :max-samples)))
  ;(if (> *samples* 1) (gl:enable :multisample))
  
  (setf *main-shader* (gficl:make-shader *main-vert* *main-frag*))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "view" (gficl:view-matrix '(5 2 3) '(-5 -2 -3) '(0 0 1)))
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix *target-width* *target-height* 1 0.1))
  (gficl:bind-matrix *main-shader* "model" (gficl:make-matrix))
  (setf *offscreen-fb*
	(gficl:make-framebuffer
	 (list (gficl:make-attachment-description :color-attachment0 :texture)
	       (gficl:make-attachment-description :depth-stencil-attachment))
	 *target-width* *target-height*))

  (setf *cube* (gficl:make-vertex-data
		(gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :float)))
		(getf *cube-data* :verts) (getf *cube-data* :indices)))
  
  (setf *post-shader* (gficl:make-shader *post-vert* *post-frag*))
  (gficl:bind-gl *post-shader*)
  (gl:uniformi (gficl:shader-loc *post-shader* "screen") 0)

  (setf *dummy-vert*
	(gficl:make-vertex-data (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int)))
				'(((0))) '(0 0 0)))
  
  (resize (gficl:window-width) (gficl:window-height)))

(defun resize (w h)
  (gficl:bind-gl *post-shader*)
  (gficl:bind-matrix *post-shader* "transform"
		     (gficl:target-resolution-matrix *target-width* *target-height* w h)))

(defun cleanup ()
  (gficl:delete-gl *cube*)
  
  (gficl:delete-gl *offscreen-fb*)
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *post-shader*)
  (gficl:delete-gl *dummy-vert*))

(defun update ()
  (gficl:with-update ()
    (gficl:map-keys-pressed
     ((:escape (glfw:set-window-should-close))
      (:f (gficl:toggle-fullscreen))))))

(defun draw ()
  (gficl:with-render
   (gficl:bind-gl *offscreen-fb*)
   (gl:viewport 0 0 *target-width* *target-height*)
   (gl:clear-color 255 0 0 0)
   (gl:clear :color-buffer :depth-buffer)
   (gl:enable :depth-test)
   (gficl:bind-gl *main-shader*)
   (gficl:draw-vertex-data *cube*)
   (gl:bind-framebuffer :framebuffer 0)
   (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
   (gl:clear-color 0 0 0 0)
   (gl:clear :color-buffer)
   (gl:disable :depth-test)
   (gficl:bind-gl *post-shader*)
   (gl:active-texture :texture0)
   (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id *offscreen-fb* 0))
   (gficl:bind-gl *dummy-vert*)
   (gl:draw-arrays :triangles 0 3)))

(defun run ()
  (gficl:with-window
   (:title "post processing"
    :width *target-width* :height *target-height* :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
