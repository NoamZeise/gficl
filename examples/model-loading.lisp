(in-package :gficl-examples.model-loading)

(defparameter *bunny-path* #p"examples/assets/bunny.obj")

(defparameter *vertex-data-form*
	      (gficl:make-vertex-form
	       (list (gficl:make-vertex-slot 3 :float)
		     (gficl:make-vertex-slot 3 :float))))

(defparameter *main-vert-code*
	      "#version 330
layout (location = 0) in vec3 vertex;
layout (location = 1) in vec3 normal;

out vec3 pos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
 vec4 world_pos = model * vec4(vertex, 1);
 pos = vec3(world_pos);
 gl_Position = projection * view * world_pos;}")

(defparameter *main-frag-code*
  "#version 330
in vec3 pos;
out vec4 colour;

void main() {
  colour = vec4(1);
}")

(defparameter *bunny* nil)
(defparameter *fb* nil)
(defparameter *main-shader* nil)

(defparameter *view* nil)

;; camera
(defparameter *forward* nil)
(defparameter *position* nil)
(defparameter *target* nil)
(defparameter *world-up* nil)

(defun setup ()
  (let* ((bunny-mesh (car (obj:extract-meshes (obj:parse (probe-file *bunny-path*))))))
    (setf *bunny* (gficl:make-vertex-data-from-vectors
		   *vertex-data-form*
		   (obj:vertex-data bunny-mesh)
		   (obj:index-data bunny-mesh))))
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "model" (gficl:scale-matrix '(5 5 5)))
  
  (setf *fb* nil)
  (resize (gficl:window-width) (gficl:window-height))
  (setf *view* (gficl:make-matrix))

  (setf *world-up* (gficl:make-vec '(0 1 0)))
  (setf *position* (gficl:make-vec'(1 -0.1 1)))
  (setf *target* (gficl:make-vec '(0 0.5 0)))
  (update-view 0)
  (gl:enable :cull-face :depth-test :multisample)
  (gl:cull-face :back))

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
  (gficl:delete-gl *bunny*)
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *fb*))

(defun update-view (dt)
  (setf *position*
	(gficl:quat-conjugate-vec (gficl:make-unit-quat (* 0.1 dt) *world-up*) *position*))
  (setf *forward* (gficl:-vec *target* *position*))
  (setf *view* (gficl::view-matrix *position* *forward* *world-up*)))

(defun update ()
  (gficl:with-update (dt)
    
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
   (gficl:draw-vertex-data *bunny*)
   (gficl:blit-framebuffers *fb* 0 (gficl:window-width) (gficl:window-height))))

(defun run ()
  (gficl:with-window
   (:title "bunny viewer" :width 600 :height 400 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
