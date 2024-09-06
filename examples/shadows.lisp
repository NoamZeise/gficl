(in-package :gficl-examples/shadows)

;;; Shadow Mapping Example
;;; ----------------------
;;; Use Z to switch between a perspective and orthographic
;;; Projection Mode for the shadow map.
;;;
;;; Use WASD to change the light position
;;; and UP/DOWN/SPACE/SHIFT keys to change the camera position
;;;
;;; Use X to toggle camera rotation

(defparameter *bunny-path* #p"examples/assets/bunny.obj")
(defparameter *cube-path* #p"examples/assets/cube.obj")
(defparameter *sphere-path* #p"examples/assets/sphere.obj")

(defparameter *plane-data*
	      (list :verts '(((-1 0 -1) (0 1 0))
			     ((1 0 -1) (0 1 0))
			     ((1 0 1) (0 1 0))
			     ((-1 0 1) (0 1 0)))
		    :indices '(0 3 2 2 1 0)))

(defparameter *main-vert-code*
	      "#version 330
layout (location = 0) in vec3 position;
layout (location = 1) in vec3 normal;

out vec3 pos;
out vec4 shadow_world_pos;
out vec3 normal_vec;

uniform mat4 model;
uniform mat4 normal_mat;
uniform mat4 view;
uniform mat4 projection;

uniform mat4 light_view_proj;

void main() {
 vec4 world_pos = model * vec4(position, 1);
 pos = vec3(world_pos);
 shadow_world_pos = (light_view_proj * vec4(pos, 1));
 normal_vec = vec3(normal_mat * vec4(normal, 1));
 gl_Position = projection * view * world_pos;}")

(defparameter *main-frag-code*
  "#version 330
in vec3 pos;
in vec4 shadow_world_pos;
in vec3 normal_vec;
out vec4 colour;

uniform vec3 cam;
uniform int shaded;

uniform vec3 light_pos;
uniform sampler2DShadow shadow;
uniform float bias_factor;

float random(vec2 co) {
   return fract(sin(dot(co.xy,vec2(12.9898,78.233))) * 43758.5453);
}

float test_shadow(vec3 shadow_pos, vec2 offset, float bias) {  
  shadow_pos.xy += offset;
  shadow_pos.z -= bias;
  return texture(shadow, shadow_pos); 
}

float in_shadow(vec3 n, vec3 l) {
  vec4 shadow_pos = shadow_world_pos;
  shadow_pos /= shadow_pos.w;
  shadow_pos += vec4(1);
  shadow_pos /= 2;
  vec3 pos = shadow_pos.xyz;
  float bias = 0.000001 * bias_factor
             + 0.000005  * bias_factor * (1.0 - dot(n, -l));
  const float D = 0.002;
  // PCF with a gaussian blur
  mat3 ker = mat3(
      1.5, 3, 1.5,
      3,   5, 3,
      1.5, 3, 1.5
  ) / 23;
  float s = 0;
  // rotate sample points randomly at every pos
  float angle = random(shadow_pos.xy);
  mat2 rot = mat2(cos(angle), -sin(angle), sin(angle), cos(angle));
  for(int x = 0; x < 3; x++)
    for(int y = 0; y < 3; y++)
      s += test_shadow(pos, rot *((x-1) * vec2(D,0) 
                                + (y-1) * vec2(0,D)), bias) 
           * ker[x][y];
  return s;
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

  float in_shadow = in_shadow(n, l);
  vec3 obj_colour = gooch(n, l, v, in_shadow) * edge_highlight(n, v);
  colour = vec4(obj_colour, 1);
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
void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;
  float d = texture(tex, uv).r;
  d = (2 * near) / (far + near - d * (far - near));  
  colour = vec4(vec3(d), 1);
}")

(defconstant +max-samples+ 8)
(defconstant +shadow-map-size+ 4096)

;;; ---- Globals ----

;; assets
(defparameter *bunny* nil)
(defparameter *plane* nil)
(defparameter *cube* nil)
(defparameter *sphere* nil)
(defparameter *dummy-data* nil)
;; shaders
(defparameter *main-shader* nil)
(defparameter *shadow-shader* nil)
(defparameter *debug-shader* nil)
;; framebuffers
(defparameter *fb* nil)
(defparameter *shadow-fb* nil)
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

;;; ---- Render Object ----

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

;;; ---- Update ----

(defun update-light-vp ()
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "light_view_proj" (gficl:*mat *light-proj* *light-view*)))

(defun set-light-projection (proj far-max bias)
  (setf *light-proj* proj)  
  (gficl:bind-gl *shadow-shader*)
  (gficl:bind-matrix *shadow-shader* "projection" *light-proj*)
  
  (gficl:bind-gl *debug-shader*)
  (gl:uniformf (gficl:shader-loc *debug-shader* "near") 1)
  (gl:uniformf (gficl:shader-loc *debug-shader* "far") far-max)
  
  (gficl:bind-gl *main-shader*)
  (gl:uniformf (gficl:shader-loc *main-shader* "bias_factor") bias)
  (update-light-vp))

(defun toggle-light-projection-mode ()
  "Switch between Orthographic and Perspective shadow map light mode."
  (setf *light-ortho-mode* (not *light-ortho-mode*))
  (if *light-ortho-mode*
      (set-light-projection (gficl:orthographic-matrix 8 -4 -4 4 80 -80) 5 20)
    (let* ((near 0.5) (edge (* near (tan (/ pi 6.0))))
	   (mat (gficl:perspective-matrix edge (- edge) (- edge) edge near)))
      (set-light-projection mat 50 1))))

(defun update-light-pos ()
  (setf *light-view* (gficl:view-matrix *light-pos* (gficl:-vec '(0 -4 0) *light-pos*) *world-up*))
  (gficl:bind-gl *shadow-shader*)
  (gficl:bind-matrix *shadow-shader* "view" *light-view*)
  (update-light-vp)
  (gficl:bind-vec *main-shader* "light_pos" *light-pos*))

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

(defun update-view (dt)
  (if *cam-move*
      (setf *position*
	    (gficl:quat-conjugate-vec (gficl:make-unit-quat (* 0.1 dt) *world-up*) *position*)))
  (setf *forward* (gficl:-vec *target* *position*))
  (setf *view* (gficl:view-matrix *position* *forward* *world-up*))
  (set-model-mat *light-ro* (gficl:translation-matrix *light-pos*))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "view" *view*)
  (gficl:bind-vec *main-shader* "cam" *position*))

(defun update ()
  (gficl:with-update (dt)    
    (gficl:map-keys-pressed
     (:escape (glfw:set-window-should-close))
     (:f (gficl:toggle-fullscreen))
     (:x (setf *cam-move* (not *cam-move*))))    
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
    (light-controls dt)
    (update-view dt)))

;;; ---- Draw ----

(defun draw-occluder-pass ()
  (gficl:bind-gl *shadow-fb*)
  (gl:enable :depth-test)
  (gl:enable :multisample)
  (gl:clear :depth-buffer)
  (gl:viewport 0 0 +shadow-map-size+ +shadow-map-size+)
  (gficl:bind-gl *shadow-shader*)
  (draw-render-obj-shadow *bunny*)
  (draw-render-obj-shadow *cube*)
  (draw-render-obj-shadow *sphere*))

(defun draw-debug ()
  (gficl:bind-gl *debug-shader*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id *shadow-fb* 0))
  (gficl:bind-gl *dummy-data*)
  (gl:draw-arrays :triangles 0 3))

(defun draw-main-scene ()
  (gficl:bind-gl *main-shader*)
  (gl:enable :depth-test)
  (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id *shadow-fb* 0))
  (gl:uniformi (gficl:shader-loc *main-shader* "shaded") 1)
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
   (draw-occluder-pass)
   (draw-main-pass)))

;;; ---- Setup / Cleanup ----

(defun setup-globals ()
  (setf *fb* nil)
  
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

(defun setup-shader-variables ()
  (gficl:bind-gl *debug-shader*)
  (gl:uniformi (gficl:shader-loc *debug-shader* "tex") 0)
  (gficl:bind-matrix *debug-shader* "transform"
		     (gficl:*mat
		      (gficl:translation-matrix '(0.7 0.7 0))
		      (gficl:scale-matrix '(0.3 0.3 0))))
  
  (gficl:bind-gl *main-shader*)
  (gl:uniformi (gficl:shader-loc *main-shader* "shadow") 0)
  (toggle-light-projection-mode)
  (update-light-pos)
  (update-view 0))

(defun setup-shaders ()
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  (setf *shadow-shader* (gficl:make-shader *shadow-vert* *shadow-frag*))  
  (setf *debug-shader* (gficl:make-shader *debug-vert* *debug-frag*))
  (setup-shader-variables))

(defun setup-framebuffers ()
  (resize (gficl:window-width) (gficl:window-height))  
  (setf *shadow-fb*
	(gficl:make-framebuffer
	 (list (gficl:make-attachment-description :depth-attachment :type :texture))
	 +shadow-map-size+ +shadow-map-size+))
  (gl:bind-texture :texture-2d (gficl:framebuffer-texture-id *shadow-fb* 0))
  (gl:tex-parameter :texture-2d :texture-compare-mode :compare-ref-to-texture))

(defun resize (w h)
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix w h (* pi 0.4) 0.1))
  (if *fb* (gficl:delete-gl *fb*))
  (setf *fb* (gficl:make-framebuffer
	      (list (gficl:make-attachment-description :color-attachment0)
		    (gficl:make-attachment-description :depth-stencil-attachment))
	      w h :samples (min +max-samples+ (gl:get-integer :max-samples)))))

(defun setup ()
  (setup-globals)
  (setup-render-objects)
  (setup-shaders)
  (setup-framebuffers)
  (gl:enable :cull-face)
  (gl:cull-face :front))

(defun cleanup ()
  (gficl:delete-gl (vertex-data *bunny*))
  (gficl:delete-gl (vertex-data *cube*))
  (gficl:delete-gl (vertex-data *sphere*))
  (gficl:delete-gl (vertex-data *plane*))
  (gficl:delete-gl *dummy-data*)
  
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *shadow-shader*)
  (gficl:delete-gl *debug-shader*)
  
  (gficl:delete-gl *fb*)
  (gficl:delete-gl *shadow-fb*))
