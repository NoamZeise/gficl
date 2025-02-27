(in-package :gficl-examples/shadows)

;;; Shadow Mapping Example
;;; ----------------------
;;; M - switch shadow map mode
;;; X - toggle camera rotation
;;; Z - switch between a perspective and orthographic projection for the shadow map
;;; WASD - change light position
;;; UP/DOWN/SPACE/SHIFT -  change camera position

;;; 3d model data filepaths
(defparameter *bunny-path* #p"examples/assets/bunny.obj")
(defparameter *cube-path* #p"examples/assets/cube.obj")
(defparameter *sphere-path* #p"examples/assets/sphere.obj")
;;; 3d model hardcoded data
(defparameter *plane-data*
	      (list :verts '(((-1 0 -1) (0 1 0))
			     ((1 0 -1) (0 1 0))
			     ((1 0 1) (0 1 0))
			     ((-1 0 1) (0 1 0)))
		    :indices '(0 3 2 2 1 0)))

;;; shadow pass shaders
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

(defparameter *vsm-vert* "
#version 330
layout (location = 0) in vec3 position;
out vec4 pos;
uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
 pos = projection * view * model * vec4(position, 1);
 gl_Position = pos;}")
(defparameter *vsm-frag* "
#version 330
in vec4 pos;
out vec4 colour;
void main() {
 float depth = pos.z/pos.w;
 depth += 1;
 depth /= 2;
 colour = vec4(depth, depth*depth, 0, 1);
}")

;;; debug shader
(defparameter *debug-vert*
	      "#version 330
out vec2 uv;
uniform mat4 transform;
void main() {
  uv = vec2(((2 - gl_VertexID) << 1) & 2, (2 - gl_VertexID) & 2);
  gl_Position = transform * vec4(uv * 2.0f - 1.0f, 0.0f, 1.0f);}")
(defparameter *debug-frag*
  "#version 330
in vec2 uv;
out vec4 colour;
uniform sampler2D tex;
uniform float near;
uniform float far;
uniform int shadow_mode;
#define MODE_BASIC 0
#define MODE_PCF 1
#define MODE_VSM 2
void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  colour = vec4(1);
  if(shadow_mode == MODE_BASIC || shadow_mode == MODE_PCF) {
    float d = texture(tex, uv).r;
    d = (2 * near) / (far + near - d * (far - near));  
    colour = vec4(vec3(d), 1);
  }
  if(shadow_mode == MODE_VSM) {
    colour = texture(tex, uv).rgba;
  }
}")
;;; main shader
(defparameter *main-vert-code*
	      "#version 330
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

out vec3 pos;
out vec4 light_space_pos;
out vec3 normal_vec;

uniform mat4 model;
uniform mat4 normal_mat;
uniform mat4 view;
uniform mat4 projection;

uniform mat4 light_view_proj;

void main() {
 vec4 world_pos = model * vec4(position, 1);
 light_space_pos = light_view_proj * world_pos;
 pos = world_pos.xyz;
 normal_vec = vec3(normal_mat * vec4(normal, 1));
 gl_Position = projection * view * world_pos;}")

(defparameter *main-frag-code*
  "#version 330
in vec3 pos;
in vec4 light_space_pos;
in vec3 normal_vec;
out vec4 colour;

uniform vec3 cam;
uniform int shaded;

uniform vec3 light_pos;
uniform sampler2DShadow shadow;
uniform float bias_factor;

uniform sampler2D vsm_shadow_map;

uniform int shadow_mode;
#define MODE_BASIC 0
#define MODE_PCF 1
#define MODE_VSM 2

vec3 correct_light_pos() {
  vec4 p = light_space_pos;
  p /= p.w;
  p += vec4(1);
  return (p/2).xyz;
}

float test_shadow(vec3 shadow_pos, vec2 offset, float bias) {  
  shadow_pos.xy += offset;
  shadow_pos.z -= bias;
  return texture(shadow, shadow_pos); 
}

float single_sample_shadow(vec3 n, vec3 l) {
  vec3 pos = correct_light_pos();
  float bias = 0.000001 * bias_factor
             + 0.000005  * bias_factor * (1.0 - dot(n, -l));
  return test_shadow(pos, vec2(0, 0), bias);
}

float random(vec2 co) {
   return fract(sin(dot(co.xy,vec2(12.9898,78.233))) * 43758.5453);
}

float pcf_shadow(vec3 n, vec3 l) {
  vec3 pos = correct_light_pos();
  float bias = 0.000001 * bias_factor
             + 0.000005  * bias_factor * (1.0 - dot(n, -l));
  const float D = 0.001;
  float s = 0;
  // rotate sample points randomly at every pos
  float angle = random(pos.xy);
  mat2 rot = mat2(cos(angle), -sin(angle), sin(angle), cos(angle));
  for(int x = 0; x < 3; x++)
    for(int y = 0; y < 3; y++)
      s += test_shadow(pos, rot *((x-1) * vec2(D,0)
                                + (y-1) * vec2(0,D)), bias) 
           * 1/9;
  return s;
}

float vsm_shadow(vec3 n, vec3 l) {
  vec3 pos = correct_light_pos();
  float depth = pos.z;
  vec2 float_vec = texture(vsm_shadow_map, pos.xy).xy;                  
  float M1 = float_vec.x;
  float M2 = float_vec.y;
  if(depth <= M1) return 1.0;
  // clamp variance to avoid numerical artifacts
  float s2 = max(abs(M2 - M1*M1), 0.000001);
  float diff = depth - M1;
  float pmax = s2 / (s2 + diff*diff);
  return pmax;
}

vec3 gooch(vec3 n, vec3 l, vec3 v, float in_shadow) {
  // shading parameters
  vec3 object_colour = vec3(1);
  vec3 cool = vec3(0.2, 0, 0.55) + 0.25*object_colour;
  vec3 warm = vec3(0.5, 0.3, 0) + 0.5*object_colour;
  vec3 highlight = vec3(1, 0.8, 0.2);
  float specular = 20.0f;

  float t = (dot(n,l) + 1)/2.0;
  vec3 r = -reflect(l,n);
  float s = clamp(pow(max(dot(r, v), 0), specular), 0, 1);
  vec3 base = mix(cool, warm,  t - 0.2*t*(1 - in_shadow));
  return mix(base, highlight, s*in_shadow);
}

vec3 edge_highlight(vec3 n, vec3 v) {
  float thickness = 0.23;
  float edge_amount = dot(n, v);
  edge_amount = clamp(edge_amount, 0, thickness)*(1/thickness);
  return vec3(edge_amount);
}

void main() {
  if(shaded == 0) {
     colour = vec4(1);
     return;
  }
 
  // lighting vecs
  vec3 n = normalize(normal_vec);
  vec3 l = normalize(light_pos - pos);
  vec3 v = normalize(cam - pos);

  float in_shadow = 0.0;

  if(shadow_mode == MODE_BASIC)
    in_shadow = single_sample_shadow(n, l);
  if(shadow_mode == MODE_PCF)
    in_shadow = pcf_shadow(n, l);
  if(shadow_mode == MODE_VSM)
    in_shadow = vsm_shadow(n, l);
  vec3 obj_colour = gooch(n, l, v, in_shadow) * edge_highlight(n, v);
  colour = vec4(obj_colour, 1);//*0.001 + vec4(vec3(in_shadow), 1);
  if(in_shadow > 1) colour = vec4(1, 0, 0, 1);
}")

;;; ---- Constants ----

(defconstant +shadow-map-size+ 2048)
(defconstant +max-samples+ 8)

;;; ---- Globals ----

;; render objects
(defparameter *bunny* nil)
(defparameter *plane* nil)
(defparameter *cube* nil)
(defparameter *sphere* nil)
(defparameter *dummy-data* nil)
;; main pass shaders
(defparameter *main-shader* nil)
(defparameter *debug-shader* nil)
;; main pass framebuffer
(defparameter *fb* nil)
;; shadow map implementations
(defparameter *current-shadow-mode* nil)
(defparameter *next-shadow-modes* nil)
(defparameter *shadow-modes* nil)
;; camera
(defparameter *forward* nil)
(defparameter *position* nil)
(defparameter *target* nil)
(defparameter *view* nil)
(defparameter *world-up* (gficl:make-vec '(0 1 0)))
(defparameter *cam-move* nil)
;; light
(defparameter *light-ortho-mode* nil)
(defparameter *light-ro* nil)
(defparameter *light-pos* nil)
(defparameter *light-view* nil)
(defparameter *light-proj* nil)

;;; ---- Main ----

(defun run ()
  (gficl:with-window
   (:title "shadows example" :width 600 :height 600 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (draw))
   (cleanup)))

;;; ---- Render Object Class ----

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

(defun draw-render-obj-shadow (ro shader)
  (gficl:bind-matrix shader "model" (model-mat ro))
  (gficl:draw-vertex-data (vertex-data ro)))

(defun load-model (path)
  (car (gficl/load:model path :vertex-form '(:position :normal))))

;;; ---- Setup / Cleanup ----

(defun setup ()
  (setup-globals)
  (setup-render-objects)
  (let ((samples (min +max-samples+ (gl:get-integer :max-samples))))
    (setf *shadow-modes*
	  (list (make-basic-shadow +shadow-map-size+)
		(make-pcf-shadow +shadow-map-size+)
		(make-vsm-shadow +shadow-map-size+ samples))))
  (next-shadow-mode)
  (setup-shaders)
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :cull-face)
  (gl:cull-face :back))

(defun setup-globals ()
  (setf *fb* nil)
  (setf *next-shadow-modes* nil)
  
  (setf *light-pos* (gficl:make-vec '(10 9 10)))
  (setf *cam-move* t)
  (setf *light-ortho-mode* nil)
  
  (setf *view* (gficl:make-matrix))
  (setf *position* (gficl:make-vec'(5 1 5)))
  (setf *target* (gficl:make-vec '(0 0 0))))

(defun setup-render-objects ()
  (setf *bunny*
	(make-render-obj
	 (load-model *bunny-path*)
	 (gficl:*mat (gficl:translation-matrix '(-1.5 0 -2)) (gficl:scale-matrix '(3 3 3)))))
  (setf *cube*
	(make-render-obj
	 (load-model *cube-path*)
	 (gficl:*mat (gficl:translation-matrix '(2 -1 1)) (gficl:scale-matrix '(1 1.5 1)))))
  
  (let ((sphere-data (load-model *sphere-path*)))
    (setf *sphere* (make-render-obj sphere-data (gficl:translation-matrix '(-2 1 1.5))))
    (setf *light-ro* (make-render-obj sphere-data (gficl:translation-matrix *light-pos*))))
  
  (let ((plane-data (gficl:make-vertex-data
		     (gficl:make-vertex-form
		      (list (gficl:make-vertex-slot 3 :float) (gficl:make-vertex-slot 3 :float)))
		     (getf *plane-data* :verts) (getf *plane-data* :indices))))
    (setf *plane* (make-render-obj plane-data
				   (gficl:*mat (gficl:translation-matrix '(0 -1.15 0))
					       (gficl:scale-matrix '(7 1 7))))))

  (setf *dummy-data*
	(gficl:make-vertex-data (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int)))
				'(((0))) '(0 0 0))))

(defun setup-shaders ()
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  (setf *debug-shader* (gficl:make-shader *debug-vert* *debug-frag*))
  (setup-shader-variables))

(defun setup-shader-variables ()
  (gficl:bind-gl *debug-shader*)
  (gl:uniformi (gficl:shader-loc *debug-shader* "tex") 0)
  (gficl:bind-matrix *debug-shader* "transform"
		     (gficl:*mat
		      (gficl:translation-matrix '(0.7 0.7 0))
		      (gficl:scale-matrix '(0.3 0.3 0))))
  
  (gficl:bind-gl *main-shader*)
  (gl:uniformi (gficl:shader-loc *main-shader* "shadow") 0)
  (gl:uniformi (gficl:shader-loc *main-shader* "vsm_shadow_map") 1)
  (toggle-light-projection-mode)
  (update-light-pos)
  (update-camera 0))

(defun resize (w h)
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix w h (* pi 0.4) 0.1))
  (if *fb* (gficl:delete-gl *fb*))
  (setf *fb* (gficl:make-framebuffer
	      (list (gficl:make-attachment-description :position :color-attachment0)
		    (gficl:make-attachment-description :position :depth-stencil-attachment))
	      w h :samples (min +max-samples+ (gl:get-integer :max-samples)))))

(defun cleanup ()
  (gficl:delete-gl (vertex-data *bunny*))
  (gficl:delete-gl (vertex-data *cube*))
  (gficl:delete-gl (vertex-data *sphere*))
  (gficl:delete-gl (vertex-data *plane*))
  (gficl:delete-gl *dummy-data*)
  
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *debug-shader*)
  (loop for map in *shadow-modes* do
	(destroy-map map))
  (gficl:delete-gl *fb*))

;;; ---- Update ----

(defun update ()
  (gficl:with-update (dt)
   (gficl:map-keys-pressed
    (:escape (glfw:set-window-should-close))
    (:f (gficl:toggle-fullscreen))
    (:m (next-shadow-mode))
    (:x (setf *cam-move* (not *cam-move*))))
   (camera-controls dt)
   (light-controls dt)))

(defun camera-controls (dt)
  (gficl:map-keys-down
   (:up (setf *position* (gficl:+vec *position* (gficl:*vec (*  0.2 dt) *forward*))))	
   (:down (setf *position* (gficl:+vec *position* (gficl:*vec (* -0.2 dt) *forward*))))
   (:space
    (setf *position*
	  (gficl:+vec *position*
		      (gficl:*vec (* 0.3 dt (gficl:magnitude *forward*)) *world-up*))))
   (:left-shift
    (setf *position*
	  (gficl:+vec *position*
		      (gficl:*vec (* -0.3 dt (gficl:magnitude *forward*)) *world-up*)))))
  (update-camera dt))

(defun update-camera (dt)
  (if *cam-move*
      (setf *position*
	    (gficl:quat-conjugate-vec (gficl:make-unit-quat (* 0.1 dt) *world-up*) *position*)))
  (setf *forward* (gficl:-vec *target* *position*))
  (setf *view* (gficl:view-matrix *position* *forward* *world-up*))
  (set-model-mat *light-ro* (gficl:translation-matrix *light-pos*))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "view" *view*)
  (gficl:bind-vec *main-shader* "cam" *position*))

(defun light-controls (dt)
  (let ((speed (* 10 dt)))
    (gficl:map-keys-down
     (:w (setf *light-pos* (gficl:+vec *light-pos* (list speed 0 0))))
     (:s (setf *light-pos* (gficl:+vec *light-pos* (list (* -1 speed) 0 0))))
     (:a (setf *light-pos* (gficl:+vec *light-pos* (list 0 0 speed))))
     (:d (setf *light-pos* (gficl:+vec *light-pos* (list 0 0 (* -1 speed)))))))
  (gficl:map-keys-pressed
   (:z (toggle-light-projection-mode)))
  (update-light-pos))

(defun toggle-light-projection-mode ()
  "Switch between Orthographic and Perspective shadow map light mode."
  (setf *light-ortho-mode* (not *light-ortho-mode*))
  (if *light-ortho-mode*
      (set-light-projection (gficl:orthographic-matrix 8 -4 -4 4 0 -50) 5 20)
    (let* ((near 0.5) (edge (* near (tan (/ pi 6.0))))
	   (mat (gficl:perspective-matrix edge (- edge) (- edge) edge near)))
      (set-light-projection mat 50 1))))

(defun update-light-pos ()
  (setf *light-view*
	(gficl:view-matrix *light-pos* (gficl:-vec '(0 -4 0) *light-pos*) *world-up*))
  (set-shader-mat *current-shadow-mode* "view" *light-view*)
  (update-light-vp)
  (gficl:bind-vec *main-shader* "light_pos" *light-pos*))

(defun set-light-projection (proj far-max bias)
  (setf *light-proj* proj)
  (set-shader-mat *current-shadow-mode* "projection" *light-proj*)
  
  (gficl:bind-gl *debug-shader*)
  (gl:uniformf (gficl:shader-loc *debug-shader* "near") 1)
  (gl:uniformf (gficl:shader-loc *debug-shader* "far") far-max)
  
  (gficl:bind-gl *main-shader*)
  (gl:uniformf (gficl:shader-loc *main-shader* "bias_factor") bias)
  (update-light-vp))

(defun update-light-vp ()
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "light_view_proj" (gficl:*mat *light-proj* *light-view*)))

(defun next-shadow-mode ()
  (if (equalp *next-shadow-modes* nil) (setf *next-shadow-modes* *shadow-modes*))
  (setf *current-shadow-mode* (car *next-shadow-modes*))
  (setf *next-shadow-modes* (cdr *next-shadow-modes*))
  (if *light-view* (set-shader-mat *current-shadow-mode* "view" *light-view*))
  (if *light-proj* (set-shader-mat *current-shadow-mode* "projection" *light-proj*))
  (format t "Current Shadow Mode: ~a~%" *current-shadow-mode*))

;;; --- Shadow Pass Classes ---

(defgeneric shadow-pass (sh)
	    (:documentation "called before occluder pass"))

(defgeneric main-draw-setup (sh)
	    (:documentation "setup main shader uniforms"))

(defgeneric destroy-map (sh)
	    (:documentation "destroy resources used by shadow map method"))

(defgeneric set-shader-mat (sh uniform-name mat)
	    (:documentation "set a matrix in the shadow shader"))

(defgeneric get-shadow-map (sh)
	    (:documentation "get shadow map texture id"))

(defclass shadow-alg ()
  ((shader)
   (map-size :initarg :map-size)
   (mode-number :initarg :mode-number :accessor shadow-mode)))

(defmethod set-shader-mat ((sh shadow-alg) uniform-name mat)
	   (gficl:bind-gl (slot-value sh 'shader))
	   (gficl:bind-matrix (slot-value sh 'shader) uniform-name mat))

(defmethod shadow-pass ((sh shadow-alg))
	   (gficl:bind-gl (slot-value sh 'shader))
	   (let ((size (slot-value sh 'map-size))) (gl:viewport 0 0 size size)))

(defmethod main-draw-setup ((sh shadow-alg))
	   (gl:uniformi (gficl:shader-loc *main-shader* "shadow_mode") (shadow-mode sh)))

(defmethod destroy-map ((sh shadow-alg))
	   (gficl:delete-gl (slot-value sh 'shader)))

(defconstant +basic-shadow-mode+ 0)
(defclass basic-shadow (shadow-alg)
  ((fb))
  (:documentation "A classic shadow map"))

(defmethod initialize-instance :after ((instance basic-shadow) &key &allow-other-keys)
	   (let ((fb (with-slots (map-size) instance
		       (gficl:make-framebuffer
			(list (gficl:make-attachment-description
			       :position :depth-attachment :type :texture))
			map-size map-size)))
		 (shader (gficl:make-shader *shadow-vert* *shadow-frag*)))
	     (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id fb 0))
	     (gl:tex-parameter :texture-2d :texture-compare-mode :compare-ref-to-texture)
	     (with-slots ((f fb) (s shader)) instance
	       (setf f fb) (setf s shader))))

(defun make-basic-shadow (map-size)
  (make-instance 'basic-shadow :mode-number +basic-shadow-mode+ :map-size map-size))

(defmethod destroy-map ((sh basic-shadow))	   
	   (gficl:delete-gl (slot-value sh 'fb))
	   (call-next-method))

(defmethod get-shadow-map ((sh basic-shadow))
	   (gficl:framebuffer-texture-id (slot-value sh 'fb) 0))

(defmethod shadow-pass ((sh basic-shadow))
	   (with-slots (shader fb) sh
	     (gficl:bind-gl fb)
	     (gl:enable :depth-test)
	     (gl:disable :multisample)
	     (gl:clear :depth-buffer)
	     (call-next-method)
	     (draw-render-obj-shadow *bunny* shader)
	     (draw-render-obj-shadow *cube* shader)
	     (draw-render-obj-shadow *sphere* shader)))

(defmethod main-draw-setup ((sh basic-shadow))
	   (gl:active-texture :texture0)
	   (gl:bind-texture :texture-2d (get-shadow-map sh))
	   (call-next-method))

(defconstant +pcf-shadow-mode+ 1)
(defclass pcf-shadow (basic-shadow)
  () (:documentation "
Percentage-Close filtering - Sample the shadow map at multiple points to find the average occluder coverage of a fragment"))

(defun make-pcf-shadow (map-size)
  (make-instance 'pcf-shadow :mode-number +pcf-shadow-mode+ :map-size map-size))

(defconstant +vsm-shadow-mode+ 2)
(defclass vsm-shadow (shadow-alg)
  ((ms-fb :initarg :ms-fb)
   (resolve-fb :initarg :resolve-fb)
   (samples :initarg :samples))
  (:documentation "Variance Shadow Map - Uses a framebuffer with float components to render the depth and squared depth to."))

(defmethod initialize-instance :after ((instance vsm-shadow) &key &allow-other-keys)
	   (with-slots (map-size samples) instance
	     (let
		 ((ms-fb
		   (gficl:make-framebuffer
		    (list (gficl:make-attachment-description
			   :position :color-attachment0
			   :internal-format :rgba32f)
			  (gficl:make-attachment-description :position :depth-attachment))
		    map-size map-size
		    :samples samples))
		  (resolve-fb
		   (gficl:make-framebuffer
		    (list (gficl:make-attachment-description
			   :position :color-attachment0
			   :internal-format :rgba32f
			   :type :texture))
		    map-size map-size))
		  (shader (gficl:make-shader *vsm-vert* *vsm-frag*)))
	       (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id resolve-fb 0))
	       (gl:tex-parameter :texture-2d :texture-min-filter :linear)
	       (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
	       (with-slots ((mf ms-fb) (f resolve-fb) (s shader)) instance
		   (setf mf ms-fb) (setf f resolve-fb) (setf s shader)))))

(defun make-vsm-shadow (map-size samples)
  (make-instance 'vsm-shadow :mode-number +vsm-shadow-mode+ :map-size map-size :samples samples))

(defmethod destroy-map ((sh vsm-shadow))
	   (with-slots (ms-fb resolve-fb shader) sh
	     (gficl:delete-gl ms-fb)
	     (gficl:delete-gl resolve-fb)
	     (call-next-method)))

(defmethod get-shadow-map ((sh vsm-shadow))
	   (gficl:framebuffer-texture-id (slot-value sh 'resolve-fb) 0))

(defmethod shadow-pass ((sh vsm-shadow))
	   (with-slots (shader (size map-size) ms-fb resolve-fb) sh
	     (gficl:bind-gl ms-fb)
	     (gl:enable :depth-test :multisample)
	     (gl:clear-color 1.0 1.0 0 0)
	     (gl:clear :color-buffer :depth-buffer)
	     (call-next-method)
	     (draw-render-obj-shadow *bunny* shader)
	     (draw-render-obj-shadow *cube* shader)
	     (draw-render-obj-shadow *sphere* shader)
	     (draw-render-obj-shadow *plane* shader)
	     (gficl:blit-framebuffers ms-fb resolve-fb size size)
	     (gl:bind-texture :texture-2d (get-shadow-map sh))
	     (gl:generate-mipmap :texture-2d)))

(defmethod main-draw-setup ((sh vsm-shadow))
	   (gl:active-texture :texture1)
	   (gl:bind-texture :texture-2d (get-shadow-map sh))
	   (call-next-method))

;;; ---- Draw ----

(defun draw-debug ()
  (gficl:bind-gl *debug-shader*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (get-shadow-map *current-shadow-mode*))
  (gl:uniformi (gficl:shader-loc *debug-shader* "shadow_mode")
	       (shadow-mode *current-shadow-mode*))
  (gficl:bind-gl *dummy-data*)
  (gl:draw-arrays :triangles 0 3))

(defun draw-main-scene ()
  (gficl:bind-gl *main-shader*)
  (gl:enable :depth-test)
  (gl:uniformi (gficl:shader-loc *main-shader* "shaded") 1)
  (main-draw-setup *current-shadow-mode*)
  (draw-render-obj *bunny*)
  (draw-render-obj *cube*)
  (draw-render-obj *sphere*)
  (draw-render-obj *plane*)
  (gl:uniformi (gficl:shader-loc *main-shader* "shaded") 0)
  (draw-render-obj *light-ro*))

(defun draw-main-pass ()
  (gficl:bind-gl *fb*)
  (gl:enable :multisample)
  (gl:clear-color 0.8 0.5 0 0)
  (gl:clear :color-buffer :depth-buffer)
  (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
  (draw-debug)
  (draw-main-scene)
  (gficl:blit-framebuffers *fb* 0 (gficl:window-width) (gficl:window-height)))

(defun draw ()
  (gficl:with-render
   (shadow-pass *current-shadow-mode*)
   (draw-main-pass)))
