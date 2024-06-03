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

(defparameter *forward* nil)

(defun setup ()
  (setf *cube* (gficl:make-vertex-data
		*vertex*
		(getf *cube-data* :verts)
		(getf *cube-data* :indices)))
  (setf *main-shader* (gficl:make-shader *main-vert-code* *main-frag-code*))
  
  (setf *projection* (gficl:make-matrix))
  (setf *view* (gficl:make-matrix))

  (setf *forward* (gficl::make-vec '(0 0 1)))
  
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
    (let ((speed (* dt 1.0)))
      (if (equalp (glfw:get-key :w) :press)
	  (setf (gficl::vec-ref *forward* 1) (+ speed (gficl::vec-ref *forward* 1))))
      (if (equalp (glfw:get-key :s) :press)
	  (setf (gficl::vec-ref *forward* 1) (- (gficl::vec-ref *forward* 1) speed)))
      (if (equalp (glfw:get-key :a) :press)
	  (setf (gficl::vec-ref *forward* 0) (- (gficl::vec-ref *forward* 0) speed)))
      (if (equalp (glfw:get-key :d) :press)
	  (setf (gficl::vec-ref *forward* 0) (+ speed (gficl::vec-ref *forward* 0))))
      (if (equalp (glfw:get-key :space) :press)
	  (setf (gficl::vec-ref *forward* 2) (+ speed (gficl::vec-ref *forward* 2))))
      (if (equalp (glfw:get-key :left-shift) :press)
	  (setf (gficl::vec-ref *forward* 2) (- (gficl::vec-ref *forward* 2) speed))))))

(defun draw ()
  (gficl:with-render
   (gl:clear :color-buffer)
   (gficl:bind-gl *main-shader*)
   (gficl:bind-matrix *main-shader* "projection"
;(gficl::angle-persp-mat (gficl:window-width) (gficl:window-height) (* pi 0.3) 0.1 1000)
		      (gficl::perspective-matrix 0.1 -0.1 -0.1 0.1 0.1 1000))
   (gficl:bind-matrix *main-shader* "view"
		      (gficl::view-matrix '(0.5 0.5 -2) *forward* '(0 1 0)))
   (gficl:bind-matrix *main-shader* "model"
		      (gficl:*-mat
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
