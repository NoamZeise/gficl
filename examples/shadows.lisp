(in-package :gficl-examples.shadows)

(defparameter *bunny-path* #p"examples/assets/bunny.obj")
(defparameter *cube-path* #p"examples/assets/cube.obj")
(defparameter *sphere-path* #p"examples/assets/sphere.obj")

(defparameter *plane-data*
	      (list :verts '(((-1 0 -1) (0 1 0))
			     ((1 0 -1) (0 1 0))
			     ((1 0 1) (0 1 0))
			     ((-1 0 1) (0 1 0)))
		    :indices '(0 3 2 2 1 0)))

(defparameter *vertex-data-form*
	      (gficl:make-vertex-form
	       (list (gficl:make-vertex-slot 3 :float)
		     (gficl:make-vertex-slot 3 :float))))

(defparameter *main-vert-code*
	      "#version 330
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

out vec3 pos;
out vec3 normal_vec;

uniform mat4 model;
uniform mat4 normal_mat;
uniform mat4 view;
uniform mat4 projection;

void main() {
 vec4 world_pos = model * vec4(position, 1);
 pos = vec3(world_pos);
 normal_vec = vec3(normal_mat * vec4(normal, 1));
 gl_Position = projection * view * world_pos;}")

(defparameter *main-frag-code*
  "#version 330
in vec3 pos;
in vec3 normal_vec;
out vec4 colour;

uniform vec3 cam;

void main() {
  // shading constants
  vec3 object_colour = vec3(1);
  vec3 cool = vec3(0.2, 0, 0.55) + 0.25*object_colour;
  vec3 warm = vec3(0.5, 0.3, 0) + 0.5*object_colour;
  vec3 highlight = vec3(1, 0.8, 0.2);
  float specular = 30.0f;
  
  vec3 n = normalize(normal_vec);
  vec3 l = -vec3(0, 0.2, -1); // light direction
  vec3 v = normalize(cam - pos);

  // gooch shading
  float t = (dot(n,l) + 1)/2.0;
  vec3 r = -reflect(l,n);
  float s = clamp(pow(dot(r,v), specular), 0, 1);
  vec3 base = mix(cool, warm,  t);
  vec3 shaded = mix(base, highlight, s);
  
  // edge outline
  float edge_amount = dot(n, v);
  float thickness = 0.23;
  edge_amount = clamp(edge_amount, 0, thickness)*(1/thickness);
  vec3 edge = vec3(edge_amount);

  colour = vec4(shaded*edge, 1);
}")

(defparameter *shadow-vert* "
#version 330
layout (location = 0) in vec3 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
 gl_Position = projection * view * model * vec4(position, 1);}")
(defparameter *shadow-frag* "
#version 330
void main() {}")

(defparameter *post-vert*
  "#version 330
out vec2 uv;
uniform mat4 transform;
void main() {
  uv = vec2((gl_VertexID << 1) & 2, gl_VertexID & 2);
  gl_Position = transform * vec4(uv * 2.0f - 1.0f, 0.0f, 1.0f);}")
(defparameter *post-frag*
  "#version 330
in vec2 uv;
out vec4 colour;
uniform sampler2D tex;
void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  float d = texture(tex, uv).r;
  const float NEAR = 0.1;
  const float FAR = 10;
  d = (2 * NEAR) / (FAR + NEAR - d * (FAR - NEAR));
  
  colour = vec4(vec3(d), 1);
}")

(defparameter *bunny* nil)
(defparameter *plane* nil)
(defparameter *cube* nil)
(defparameter *sphere* nil)

(defparameter *fb* nil)
(defparameter *main-shader* nil)
(defparameter *shadow-fb* nil)
(defparameter *shadow-shader* nil)

(defparameter *debug-shader* nil)
(defparameter *dummy-vert* nil)

(defconstant +max-samples+ 8)

(defparameter *view* nil)
;; camera
(defparameter *forward* nil)
(defparameter *position* nil)
(defparameter *target* nil)
(defparameter *world-up* (gficl:make-vec '(0 1 0)))

(defclass render-obj ()
  ((vertex-data :initarg :vertex-data :accessor vertex-data :type gficl:vertex-data)
   (model-mat :accessor model-mat :type gficl:matrix :initform (gficl:make-matrix))
   (normal-mat :accessor normal-mat :type gficl:matrix :initform (gficl:make-matrix))))

(defun set-model-mat (ro model-mat)
  (setf (slot-value ro 'model-mat) model-mat)
  (setf (slot-value ro 'normal-mat) (gficl:transpose-matrix (gficl:inverse-matrix model-mat))))

(defun make-render-obj (vertex-data model-mat)
  (let ((ro (make-instance 'render-obj :vertex-data vertex-data)))
    (set-model-mat ro model-mat) ro))

(defun draw-render-obj (ro)
  (gficl:bind-matrix *main-shader* "model" (model-mat ro))
  (gficl:bind-matrix *main-shader* "normal_mat" (normal-mat ro))
  (gficl:draw-vertex-data (vertex-data ro)))

(defun draw-render-obj-shadow (ro)
  (gficl:bind-matrix *shadow-shader* "model" (model-mat ro))
  (gficl:draw-vertex-data (vertex-data ro)))

(defun load-model (path)
  (car (gficl/load:model path :vertex-form '(:position :normal))))

(defun setup ()  
  (setf *bunny*
	(make-render-obj
	 (load-model *bunny-path*)
	 (gficl:*mat
	  (gficl:translation-matrix '(-1.5 0 -2))
	  (gficl:scale-matrix '(3 3 3)))))
  (setf *cube*
	(make-render-obj
	 (load-model *cube-path*)
	 (gficl:*mat
	  (gficl:translation-matrix '(2 -1 1))
	  (gficl:scale-matrix '(1 1.5 1)))))
  (setf *sphere*
	(make-render-obj
	 (load-model *sphere-path*)
	 (gficl:translation-matrix '(-2 1 1.5))))
  (setf *plane*
	(make-render-obj
	 (gficl:make-vertex-data
	  *vertex-data-form*
	  (getf *plane-data* :verts) (getf *plane-data* :indices))
	 (gficl:*mat
	  (gficl:translation-matrix '(0 -1.15 0))
	  (gficl:scale-matrix '(7 1 7)))))

  (setf *dummy-vert*
	(gficl:make-vertex-data (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int)))
				'(((0))) '(0 0 0)))
  
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  (setf *shadow-shader* (gficl:make-shader *shadow-vert* *shadow-frag*))
  
  (setf *debug-shader* (gficl:make-shader *post-vert* *post-frag*))
  (gficl:bind-gl *debug-shader*)
  (gl:uniformi (gficl:shader-loc *debug-shader* "tex") 0)
  (gficl:bind-matrix *debug-shader* "transform"
		     (gficl:*mat
		      (gficl:translation-matrix '(0.7 0.7 0))
		      (gficl:scale-matrix '(0.3 0.3 0))))

  (gficl:bind-gl *main-shader*)  
  (setf *fb* nil)
  (setf *shadow-fb* nil)
  (resize (gficl:window-width) (gficl:window-height))
  (setf *view* (gficl:make-matrix))

  (setf *position* (gficl:make-vec'(5 1 5)))
  (setf *target* (gficl:make-vec '(0 0 0)))
  (update-view 0)
  (gl:clear-color 0.8 0.5 0 0)
  (gl:enable :cull-face :depth-test :multisample)
  (gl:cull-face :front))

(defun resize (w h)
  (let ((proj (gficl::screen-perspective-matrix w h (* pi 0.4) 0.1)))
    (gficl:bind-gl *main-shader*)
    (gficl:bind-matrix *main-shader* "projection" proj)
    (gficl:bind-gl *shadow-shader*)
    (gficl:bind-matrix *shadow-shader* "projection" proj))
  (let ((samples (min +max-samples+ (gl:get-integer :max-samples))))
    (if *fb* (gficl:delete-gl *fb*))
    (setf *fb* (gficl:make-framebuffer
		(list (gficl:make-attachment-description :color-attachment0)
		      (gficl:make-attachment-description :depth-stencil-attachment))
		w h :samples samples))
    (if *shadow-fb* (gficl:delete-gl *shadow-fb*))
    (setf *shadow-fb*
	  (gficl:make-framebuffer
	   (list (gficl:make-attachment-description :depth-attachment :texture)) w h))))

(defun cleanup ()
  (gficl:delete-gl (vertex-data *bunny*))
  (gficl:delete-gl (vertex-data *cube*))
  (gficl:delete-gl (vertex-data *sphere*))
  (gficl:delete-gl (vertex-data *plane*))
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *shadow-shader*)

  (gficl:delete-gl *debug-shader*)
  (gficl:delete-gl *dummy-vert*)
  
  (gficl:delete-gl *fb*)
  (gficl:delete-gl *shadow-fb*))

(defun update-view (dt)
  (setf *position*
	(gficl:quat-conjugate-vec (gficl:make-unit-quat (* 0.1 dt) *world-up*) *position*))
  (setf *forward* (gficl:-vec *target* *position*))
  (setf *view* (gficl::view-matrix *position* *forward* *world-up*)))

(defun update ()
  (gficl:with-update (dt)
    
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen)))
    
    (gficl:map-keys-down
     (:up (setf *position*   (gficl:+vec *position* (gficl:*vec (*  0.2 dt) *forward*))))	
     (:down (setf *position* (gficl:+vec *position* (gficl:*vec (* -0.2 dt) *forward*))))
     (:space
      (setf *position*
	    (gficl:+vec *position*
			(gficl:*vec (* 0.3 dt (gficl:magnitude *forward*)) *world-up*))))
     (:left-shift
      (setf *position*
	    (gficl:+vec *position*
			(gficl:*vec (* -0.3 dt (gficl:magnitude *forward*)) *world-up*)))))      
    (update-view dt)))

(defun draw ()
  (gficl:with-render

   ;; depth map render
   (gficl:bind-gl *shadow-fb*)
   (gl:disable :multisample)
   (gl:clear :depth-buffer)
   (gficl:bind-gl *shadow-shader*)
   (gficl:bind-matrix *shadow-shader* "view" *view*)
   (draw-render-obj-shadow *bunny*)
   (draw-render-obj-shadow *cube*)
   (draw-render-obj-shadow *sphere*)
   (draw-render-obj-shadow *plane*)

   ;; main render
   (gficl:bind-gl *fb*)
   (gl:enable :multisample)
   (gl:clear :color-buffer :depth-buffer)
   
   (gficl:bind-gl *debug-shader*)
   (gl:disable :cull-face)
   (gl:active-texture :texture0)
   (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id *shadow-fb* 0))
   (gficl:bind-gl *dummy-vert*)
   (gl:draw-arrays :triangles 0 3)
   
   (gficl:bind-gl *main-shader*)
   (gl:enable :cull-face)
   (gficl:bind-matrix *main-shader* "view" *view*)
   (gficl:bind-vec *main-shader* "cam" *position*)
   (draw-render-obj *bunny*)
   (draw-render-obj *cube*)
   (draw-render-obj *sphere*)
   (draw-render-obj *plane*)
   (gficl:blit-framebuffers *fb* 0 (gficl:window-width) (gficl:window-height))))

(defun run ()
  (gficl:with-window
   (:title "shadows example" :width 600 :height 600 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
