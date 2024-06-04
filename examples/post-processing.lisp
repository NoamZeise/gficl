(in-package :gficl-examples.post-processing)

(defparameter *vertex*
  (gficl:make-vertex-form
   (list (gficl:make-vertex-slot 3 :float)
	 (gficl:make-vertex-slot 2 :float))))

(defparameter *cube-data*
  (list :verts
	'(((0 0 0) (0 0))
	  ((1 0 0) (1 0))
	  ((1 1 0) (1 1))
	  ((0 1 0) (0 1)))
	:indices
	'(0 3 2 2 1 0)))

(defparameter *main-vert-code*
  "#version 330
layout (location = 0) in vec3 vertex;
layout (location = 1) in vec2 inTex;

out vec2 outTex;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main() {
 outTex = inTex;
 gl_Position = projection * view * model * vec4(vertex, 1);
 gl_Position /= gl_Position.w;
}")
(defparameter *main-frag-code*
  "#version 330
in vec2 tex;
out vec4 colour;

void main() {
  colour = vec4(1, 0, 1, 1);
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
  
  (setf *projection* (gficl:make-matrix))
  (setf *view* (gficl:make-matrix))

  (setf *forward* (gficl:make-vec '(0 0 1)))
  (setf *up* (gficl:make-vec '(0 1 0)))
  (setf *left* (gficl:make-vec '(1 0 0)))
  (setf *position* (gficl:make-vec'(0.5 0.5 -2)))
  (setf *model* (gficl:make-matrix)))

(defun cleanup ()
  (gficl:delete-gl *cube*)
  (gficl:delete-gl *main-shader*))

(defparameter *pressed-last* nil)

(defun update ()
  (gficl:with-update (dt)
    (if (equalp (glfw:get-key :escape) :press)
	(glfw:set-window-should-close))
    (if (equalp (glfw:get-key :f) :press)
	(if (not *pressed-last*)
	    (progn (gficl:toggle-fullscreen)
		   (setf *pressed-last* t)))
      (setf *pressed-last* nil))
    (let ((speed (* dt 1.0))
	  (ctrl (gficl:make-vec '(0 0))))
      
      (if (equalp (glfw:get-key :up) :press)
	  (setf *position*
		(gficl:+vec *position* (gficl:*vec speed *forward*))))
      (if (equalp (glfw:get-key :down) :press)
	  (setf *position*
		(gficl:+vec *position* (gficl:*vec (- speed) *forward*))))
      (if (equalp (glfw:get-key :left) :press)
	  (setf *position*
		(gficl:+vec *position* (gficl:*vec (- speed) *left*))))
      (if (equalp (glfw:get-key :right) :press)
	  (setf *position*
		(gficl:+vec *position* (gficl:*vec speed *left*))))
      (if (equalp (glfw:get-key :space) :press)
	  (setf *position*
		(gficl:+vec *position* (list 0 speed 0))))
      (if (equalp (glfw:get-key :left-shift) :press)
	  (setf *position*
		(gficl:+vec *position* (list 0 (- speed) 0))))
      
      (if (equalp (glfw:get-key :w) :press)
	  (setf ctrl (gficl:+vec ctrl '(0 -1))))
      (if (equalp (glfw:get-key :s) :press)
	  (setf ctrl (gficl:+vec ctrl '(0 1))))
      (if (equalp (glfw:get-key :a) :press)
	  (setf ctrl (gficl:+vec ctrl '(-1 0))))
      (if (equalp (glfw:get-key :d) :press)
	  (setf ctrl (gficl:+vec ctrl '(1 0))))

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
   (gl:clear :color-buffer)
   (gficl:bind-gl *main-shader*)
   (gficl:bind-matrix *main-shader* "projection"
     (gficl::screen-perspective-matrix (gficl:window-width) (gficl:window-height) (* pi 0.3) 0.1))
   (gficl:bind-matrix *main-shader* "view" *view*)
   (gficl:bind-matrix *main-shader* "model"
     (gficl:*mat
      (gficl:translation-matrix '(0 0 0))
      (gficl:scale-matrix '(1 1 1))))
   (gficl:draw-vertex-data *cube*)))

(defun run ()
  (gficl:with-window
   (:title "post-processing" :width 600 :height 400)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (draw))
   (cleanup)))
