(in-package :gficl-examples.post-processing)

(defparameter *vertex*
  (gficl:make-vertex-form
   (list (gficl:make-vertex-slot 3 :float)
	 (gficl:make-vertex-slot 2 :float))))

(defparameter *cube-data*
  (list :verts
	'(((-1 -1 -1) (0 0))
	  ((-1 -1  1) (1 0))
	  ((-1  1 -1) (1 1))
	  ((-1  1  1) (0 1))
	  (( 1 -1 -1) (0 0))
	  (( 1 -1  1) (1 0))
	  (( 1  1 -1) (1 1))
	  (( 1  1  1) (0 1)))
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
layout (location = 1) in vec2 inTex;

out vec2 outTex;
out vec3 pos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
 outTex = inTex;
 pos = vec3(model * vec4(vertex, 1));
 gl_Position = projection * view * vec4(pos, 1);
}")
(defparameter *main-frag-code*
  "#version 330
in vec2 tex;
in vec3 pos;
out vec4 colour;

void main() {
  colour = vec4(sin(1.5*pos.x), cos(1.5*pos.y), tan(1.5*pos.z), 1);
}")

(defparameter *post-vert-code* nil)
(defparameter *post-frag-code* nil)

(defparameter *cube* nil)

(defparameter *fb* nil)

(defparameter *main-shader* nil)
(defparameter *post-shader* nil)

(defparameter *projection* nil)
(defparameter *view* nil)
(defparameter *model* nil)

;; camera
(defparameter *forward* nil)
(defparameter *left* nil)
(defparameter *up* nil)
(defparameter *position* nil)

(defun setup ()
  (setf *cube* (gficl:make-vertex-data
		*vertex*
		(getf *cube-data* :verts)
		(getf *cube-data* :indices)))
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  
  (resize (gficl:window-width) (gficl:window-height))
  (setf *view* (gficl:make-matrix))

  (setf *up* (gficl:make-vec '(0 1 0)))
  (setf *left* (gficl:make-vec '(1 0 0)))
  (setf *position* (gficl:make-vec'(5 4 5)))
  (setf *forward* (gficl:-vec '(0 0 0) *position*))
  (setf *model* (gficl:make-matrix))
  (gl:enable :blend :cull-face :depth-test))

(defun resize (w h)
  (setf *projection* (gficl::screen-perspective-matrix w h (* pi 0.3) 0.1)))

(defun cleanup ()
  (gficl:delete-gl *cube*)
  (gficl:delete-gl *main-shader*))

(defparameter *pressed-last* nil)

(defun update ()
  (gficl:with-update (dt)
    
    (gficl:if-key :escape (glfw:set-window-should-close))
    (gficl:if-key :f (if (not *pressed-last*)
			 (progn (gficl:toggle-fullscreen)
				(setf *pressed-last* t)))
		  (setf *pressed-last* nil))
    (let ((speed (* dt 1.0))
	  (ctrl (gficl:make-vec '(0 0))))

      ;; movement
      (gficl:map-keys
       ((:up (setf *position*    (gficl:+vec *position* (gficl:*vec speed *forward*))))	
	(:down (setf *position*  (gficl:+vec *position* (gficl:*vec (- speed) *forward*))))	
	(:left (setf *position*  (gficl:+vec *position* (gficl:*vec (- speed) *left*))))
	(:right (setf *position* (gficl:+vec *position* (gficl:*vec speed *left*))))
	(:space (setf *position*      (gficl:+vec *position* (list 0 speed 0))))
	(:left-shift (setf *position* (gficl:+vec *position* (list 0 (- speed) 0))))))

      ;; view
      (gficl:map-keys
       ((:w (setf ctrl (gficl:+vec ctrl '(0 -1))))
	(:s (setf ctrl (gficl:+vec ctrl '(0 1))))
	(:a (setf ctrl (gficl:+vec ctrl '(-1 0))))
	(:d (setf ctrl (gficl:+vec ctrl '(1 0))))))

      (let ((ctrl (gficl:*vec (* dt 0.5) ctrl)))
	(setf *forward*
	      (gficl:normalise
	       (gficl:quat-conjugate-vec
		(gficl:*quat
		 (gficl:make-unit-quat (gficl:vec-ref ctrl 0) *up*)
		 (gficl:make-unit-quat (gficl:vec-ref ctrl 1) *left*))
		*forward*))))

      (multiple-value-setq (*view* *up* *left*)
			   (gficl::view-matrix *position* *forward* '(0 1 0))))))

(defun draw ()
  (gficl:with-render
   (gl:clear :color-buffer :depth-buffer)
   (gficl:bind-gl *main-shader*)
   (gficl:bind-matrix *main-shader* "projection" *projection*)
   (gficl:bind-matrix *main-shader* "view" *view*)
   (loop for x from -5 to 5 do
	 (loop for y from -5 to 5 do
	       (gficl:bind-matrix *main-shader* "model"
				  (gficl:*mat
				   (gficl:translation-matrix (list (* x 5) 0 (* y 5)))
				   (gficl:scale-matrix '(1 1 1))))
	       (gficl:draw-vertex-data *cube*)))))

(defun run ()
  (gficl:with-window
   (:title "post-processing" :width 600 :height 400 :resize-callback #'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
