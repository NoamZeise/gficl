(in-package :gficl-examples/pbr)

;;; Work in progress physically based lighting example

(defun run ()
  (gficl:with-window
   (:title "pbr" :resize-callback #'resize)
   (setup)
   (loop until (gficl:closedp)
	 do (update)
	 do (draw))
   (cleanup)))

(defun setup ()
  (setf *shader* (gficl:make-shader *vert* *frag*))
  (gficl:bind-gl *shader*)
  (setf *sphere* (car (gficl/load:model +sphere-path+ :vertex-form '(:position :normal))))
  (setf *cam-pos* (gficl:make-vec '(2 0 2)))
  (setf *cam-target* (gficl:make-vec '(0 0 0)))  
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test))

(defun cleanup ()
  (gficl:delete-gl *shader*)
  (gficl:delete-gl *sphere*))

(defun resize (w h)
  (setf *projection-mat* (gficl:screen-perspective-matrix w h (* pi 0.3) 0.05)))

(defun update ()
  (gficl:with-update
   (dt)
   (if (gficl:key-pressed :escape) (glfw:set-window-should-close))
   (if (gficl:key-pressed :f) (gficl:toggle-fullscreen))
   (setf *cam-pos* (gficl:rotate-vec *cam-pos* (* dt 0.1) *world-up*))
   (setf *view-mat*
	 (gficl:view-matrix *cam-pos* (gficl:-vec *cam-target* *cam-pos*) *world-up*))
   (gficl:bind-matrix *shader* "viewproj" (gficl:*mat *projection-mat* *view-mat*))
   (gficl:bind-vec *shader* "cam" *cam-pos*)))

(defun draw ()
  (gficl:with-render
   (gl:clear :color-buffer :depth-buffer)
   (let* ((model (gficl:make-matrix))
	  (norm (gficl:transpose-matrix (gficl:inverse-matrix model))))
     (gficl:bind-matrix *shader* "model" model)
     (gficl:bind-matrix *shader* "norm_mat" norm)
     (gficl:draw-vertex-data *sphere*))))

;; Global Variables

(defparameter *shader* nil)

(defparameter *cam-pos* nil)
(defparameter *cam-target* nil)
(defparameter *view-mat* nil)
(defparameter *projection-mat* nil)

(defparameter *sphere* nil)

;; Hardcoded Data

(defparameter *world-up* (gficl:make-vec'(0 1 0)))

(defparameter *vert* "
#version 330

layout (location = 0) in vec3 pos;
layout (location = 1) in vec3 normal;

uniform mat4 model;
uniform mat4 norm_mat;
uniform mat4 viewproj;

out vec3 fpos;
out vec3 fnorm;

void main() {
  vec4 world = model * vec4(pos, 1);
  fpos = vec3(world);
  fnorm = vec3(norm_mat * vec4(normal, 1));
  gl_Position = viewproj * world;
}")
(defparameter *frag* "
#version 330

in vec3 fpos;
in vec3 fnorm;
out vec4 colour;

uniform vec3 cam;

#define PI 3.14159265358979

vec4 lambertian() {
 vec4 c_obj = vec4(1, 0, 0, 1);
 return c_obj * (1/PI);
}

vec4 brdf(vec3 light, vec3 view) {
   return lambertian();
}

void main() {
  vec3 normal = normalize(fnorm);
  vec3 view = normalize(fpos - cam);
  
  vec3 light = normalize(vec3(2, 3, 1));
  vec4 c_light = vec4(1);

  colour = PI * brdf(light, view) * c_light * clamp(dot(normal,light), 0, 1);
}")

(defconstant +sphere-path+ #p"examples/assets/sphere.obj")
