(in-package :gficl-examples.cube-wave)

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

(defparameter *main-vert-code*
  "#version 330
layout (location = 0) in vec3 vertex;

out vec3 pos;
out vec3 localpos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

uniform int dim;
uniform float time;

float height(float x, float y) {
 return 0.4*length(vec3(x, 0, y))
      + 0.4*cos(4*time + (x * 0.7))
      + 0.4*sin(4*time + (y * 0.7)) 
      + 1*sin(1*time + (x * 0.3))
      + 3*sin(0.4*time + (x * 0.1))
      + 3*cos(0.4*time + (y * 0.1))
      - 14;
}

void main() {
 localpos = vec3(model * vec4(vertex, 1));
 int x = 2 * (gl_InstanceID / dim) - dim;
 int y = 2 * (gl_InstanceID % dim) - dim;
 pos = vec3(x*0.98, height(x, y), y*0.98) + localpos;
 gl_Position = projection * view * vec4(pos, 1);}")

(defparameter *main-frag-code*
  "#version 330
in vec3 pos;
in vec3 localpos;
out vec4 colour;

void main() {
  colour = vec4(localpos.y * 0.2 - 0.45);
  colour += vec4(sin(0.05*pos.x), 2*sin(0.02*pos.y), sin(0.05*pos.z), 1);
  colour *= sinh(localpos.x - localpos.z)*0.04 + 0.9;
}")

(defparameter *cube* nil)
(defparameter *fb* nil)
(defparameter *main-shader* nil)

(defparameter *view* nil)

;; camera
(defparameter *forward* nil)
(defparameter *position* nil)
(defparameter *target* nil)
(defparameter *world-up* nil)

(defparameter *time* nil)
(defparameter *cubes-dim* nil)

(defun setup ()
  (setf *cube* (gficl:make-vertex-data
		(gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :float)))
		(getf *cube-data* :verts) (getf *cube-data* :indices)))
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  (setf *cubes-dim* 50)
  (gficl:bind-gl *main-shader*)
  (gl:uniformi (gficl:shader-loc *main-shader* "dim") *cubes-dim*)
  (gficl:bind-matrix *main-shader* "model" (gficl:scale-matrix '(1 5 1)))
  
  (setf *fb* nil)
  (resize (gficl:window-width) (gficl:window-height))
  (setf *view* (gficl:make-matrix))

  (setf *world-up* (gficl:make-vec '(0 1 0)))
  (setf *position* (gficl:make-vec'(20 14 20)))
  (setf *target* (gficl:make-vec '(0 -20 0)))
  (setf *time* 0)
  (update-view 0)
  (gl:enable :cull-face :depth-test :multisample))

(defun resize (w h)
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
    (gficl::screen-perspective-matrix w h (* pi 0.4) 0.1))
  (if *fb* (gficl:delete-gl *fb*))
  (setf *fb* (gficl:make-framebuffer
	      (list (gficl:make-attachment-description :color-attachment0)
		    (gficl:make-attachment-description :depth-stencil-attachment))
	      w h (min 4 (gl:get-integer :max-samples)))))

(defun cleanup ()
  (gficl:delete-gl *cube*)
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *fb*))

(defun update-view (dt)
  (setf *position*
	(gficl:quat-conjugate-vec (gficl:make-unit-quat (* 0.1 dt) *world-up*) *position*))
  (setf *forward* (gficl:-vec *target* *position*))
  (setf *view* (gficl::view-matrix *position* *forward* *world-up*)))

(defun update ()
  (gficl:with-update (dt)
    (setf *time* (+ *time* dt))
    
    (gficl:map-keys-pressed
     ((:escape (glfw:set-window-should-close))
      (:f (gficl:toggle-fullscreen))))
    
    (gficl:map-keys-down
     ((:up (setf *position*   (gficl:+vec *position* (gficl:*vec (*  0.2 dt) *forward*))))	
      (:down (setf *position* (gficl:+vec *position* (gficl:*vec (* -0.2 dt) *forward*))))
      (:space
       (setf *position*
	     (gficl:+vec *position*
			 (gficl:*vec (* 0.3 dt (gficl:magnitude *forward*)) *world-up*))))
      (:left-shift
       (setf *position*
	     (gficl:+vec *position*
			 (gficl:*vec (* -0.3 dt (gficl:magnitude *forward*)) *world-up*))))))      
    (update-view dt)))

(defun draw ()
  (gficl:with-render
   (gficl:bind-gl *fb*)
   (gl:clear :color-buffer :depth-buffer)
   (gficl:bind-gl *main-shader*)
   (gficl:bind-matrix *main-shader* "view" *view*)
   (gl:uniformf (gficl:shader-loc *main-shader* "time") *time*)
   (gficl:draw-vertex-data *cube* :instances (* *cubes-dim* *cubes-dim*))
   (gficl:blit-framebuffers *fb* 0 (gficl:window-width) (gficl:window-height))))

(defun run ()
  (gficl:with-window
   (:title "cube-waves" :width 600 :height 400 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
