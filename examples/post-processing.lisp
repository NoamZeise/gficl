(in-package :gficl-examples.post-processing)

(defparameter *main-vert*
  "#version 330
layout (location = 0) in vec3 position;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

out vec3 pos;

void main() {
  pos = position;
  gl_Position = projection * view * model * vec4(pos, 1);
}")

(defparameter *main-frag*
  "#version 330
in vec3 pos;
out vec4 colour;

void main() {
   vec3 p = (pos + vec3(3)) * 0.2;
   colour = vec4(p.x, p.y, p.z, 1);
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
void main() { 
  if(uv.x > 1 || uv.y > 1 || uv.x < 0 || uv.y < 0) discard;

  // posterise
  colour = texture(screen, uv);
  int colour_count = 8;
  colour *= colour_count;
  for(int i = 0; i < 4; i++)
    colour[i] = floor(colour[i]);
  colour /= colour_count;
  
  // edge detection
  mat3 edge_ker = mat3(
      1,  1, 1,
      1, -8, 1,
      1,  1, 1
  );  
  float offset = 1.0/200;
  float edge_intensity = 0;
  for(int x = 0; x < 3; x++) {
    for(int y = 0; y < 3; y++) {
      vec4 col = texture(screen, uv + (x-1) * vec2(offset,0) 
                                    + (y-1) * vec2(0,offset));
      edge_intensity += ((col.r + col.g + col.b)/3) * edge_ker[x][y];
    }
  } 
  edge_intensity = step(0.1, edge_intensity);
  colour *= vec4(1) - vec4(edge_intensity);

  // dot pattern
  float amount = 800;
  colour *= 1.1;
  colour += 0.1f;
  colour *= step(0.1, sin(uv.x*amount*1.4) + cos(uv.y*amount))*0.6 + 0.4;
}")

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
(defparameter *target-width* 1000)
(defparameter *target-height* 700)
(defparameter *fixed-target* nil)
(defparameter *offscreen-w* nil)
(defparameter *offscreen-h* nil)
(defparameter *main-shader* nil)
(defparameter *offscreen-fb* nil)
(defparameter *resolve-fb* nil)

;; post-processing 
(defparameter *dummy-vert* nil)
(defparameter *post-shader* nil)

;; scene
(defparameter *cube* nil)
(defparameter *world-up* (gficl:make-vec '(0 0 1)))
(defparameter *position* nil)
(defparameter *target* nil)

(defun setup ()
  (setf *fixed-target* t)
  (setf *resolve-fb* nil)
  (setf *offscreen-fb* nil)
  (setf *samples* (min 8 (gl:get-integer :max-samples)))  
  (setf *main-shader* (gficl:make-shader *main-vert* *main-frag*))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "model" (gficl:make-matrix))

  (setf *cube* (gficl:make-vertex-data
		(gficl:make-vertex-form (list (gficl:make-vertex-slot 3 :float)))
		(getf *cube-data* :verts) (getf *cube-data* :indices)))
  
  (setf *post-shader* (gficl:make-shader *post-vert* *post-frag*))
  (gficl:bind-gl *post-shader*)
  (gl:uniformi (gficl:shader-loc *post-shader* "screen") 0)

  (setf *dummy-vert*
	(gficl:make-vertex-data (gficl:make-vertex-form (list (gficl:make-vertex-slot 1 :int)))
				'(((0))) '(0 0 0)))

  (setf *position* (gficl:make-vec '(5 2 3)))
  (setf *target* (gficl:make-vec '(0 0 0)))
  (if *fixed-target* (make-offscreen *target-width* *target-height*))
  (resize (gficl:window-width) (gficl:window-height)))

(defun resize (w h)
  (if (not *fixed-target*) (make-offscreen w h))
  (gficl:bind-gl *post-shader*)
  (gficl:bind-matrix *post-shader* "transform"
		     (gficl:target-resolution-matrix *offscreen-w* *offscreen-h* w h)))

(defun make-offscreen (w h)
  (setf *offscreen-w* w)
  (setf *offscreen-h* h)
  (if *resolve-fb* (gficl:delete-gl *resolve-fb*))
  (setf *resolve-fb* nil)
  (if (> *samples* 1)
      (setf *resolve-fb*
	    (gficl:make-framebuffer
	     (list (gficl:make-attachment-description :color-attachment0 :texture))
	     w h)))
  (if *offscreen-fb* (gficl:delete-gl *offscreen-fb*))
  (setf *offscreen-fb* nil)
  (setf *offscreen-fb*
	(gficl:make-framebuffer
	 (list (gficl:make-attachment-description :color-attachment0
						  (if *resolve-fb* :renderbuffer :texture))
	       (gficl:make-attachment-description :depth-stencil-attachment))
	 w h *samples*))
  (gficl:bind-gl *main-shader*)
  (gficl:bind-matrix *main-shader* "projection"
		     (gficl:screen-perspective-matrix w h 1 0.1)))

(defun cleanup ()
  (gficl:delete-gl *cube*)
  (if *resolve-fb* (gficl:delete-gl *resolve-fb*))
  (gficl:delete-gl *offscreen-fb*)  
  (gficl:delete-gl *main-shader*)
  (gficl:delete-gl *post-shader*)
  (gficl:delete-gl *dummy-vert*))

(defun update ()
  (gficl:with-update (dt)
    (gficl:map-keys-pressed
     ((:escape (glfw:set-window-should-close))
      (:f (gficl:toggle-fullscreen))))
    (gficl:bind-gl *main-shader*)
    (setf *position* (gficl:rotate-vec *position* (* dt 0.3) *world-up*))    
    (gficl:bind-matrix *main-shader* "view"
		       (gficl:view-matrix *position* (gficl:-vec '(0 0 0) *position*) *world-up*))))

(defun draw ()
  (gficl:with-render
   (draw-offscreen)
   (draw-post)))

(defun draw-offscreen ()
  (gficl:bind-gl *offscreen-fb*)
  (gl:viewport 0 0 *offscreen-w* *offscreen-h*)
  (gl:clear-color 0.4 0.5 0 0)
  (gl:clear :color-buffer :depth-buffer)
  (gl:enable :depth-test)
  (if *resolve-fb* (gl:enable :multisample))
  (gficl:bind-gl *main-shader*)
  (gficl:draw-vertex-data *cube*)
  (if *resolve-fb*
      (gficl:blit-framebuffers *offscreen-fb* *resolve-fb* *offscreen-w* *offscreen-h*)))

(defun draw-post ()
  (gl:bind-framebuffer :framebuffer 0)
  (gl:viewport 0 0 (gficl:window-width) (gficl:window-height))
  (gl:clear-color 0 0 0 0)
  (gl:clear :color-buffer)
  (gl:disable :depth-test :multisample)
  (gficl:bind-gl *post-shader*)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d
		   (gficl:framebuffer-texture-id (if *resolve-fb* *resolve-fb* *offscreen-fb*) 0))
  (gficl:bind-gl *dummy-vert*)
  (gl:draw-arrays :triangles 0 3))

(defun run ()
  (gficl:with-window
   (:title "post processing"
	   :width *target-width* :height *target-height* :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
