(in-package :gficl-examples/quad-spin)

(defparameter *samples* 1)

(defparameter *fb-attachments*
  (list (gficl:make-attachment-description)
	(gficl:make-attachment-description :position :depth-stencil-attachment)))
(defparameter *fb* nil)

;; shader
(defparameter *shader* nil)
;; shader input
(defparameter *vertex-format*
  (gficl:make-vertex-form
   (list (gficl:make-vertex-slot 2 :float)
	 (gficl:make-vertex-slot 2 :float))))
;; shader code
(defparameter *vert-shader*
  "#version 330
layout (location = 0) in vec2 vertex;
layout (location = 1) in vec2 inTexCoords;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 projection;

void main() {
    TexCoords = inTexCoords;
    gl_Position = projection * model * vec4(vertex, 0, 1);
}")
(defparameter *frag-shader*
  "#version 330

in vec2 TexCoords;
out vec4 colour;

uniform sampler2D tex;

void main() {
  colour = texture(tex, TexCoords);
}")
;; shader data
(defparameter *projection* nil)

;; object data
(defparameter *quad* nil)

(defparameter *tex* nil)
(defparameter *model* nil)
(defparameter *rot* nil)

(defparameter *bg-tex* nil)
(defparameter *bg-model* nil)

(defun setup ()
  (setf *shader* (gficl:make-shader *vert-shader* *frag-shader*))
  (gl:clear-color 0 0 0 0)
  (setf *samples* (min 16 (gl:get-integer :max-samples)))
  (if (> *samples* 1) (gl:enable :multisample))
  (setf *fb* nil)
  
  (resize (gficl:window-width) (gficl:window-height))
  (gl:enable :depth-test)
  
  (setf *quad*
	(gficl:make-vertex-data
	 *vertex-format*
	 '(((0 0) (0 0))
	   ((1 0) (1 0))
	   ((1 1) (1 1))
	   ((0 1) (0 1)))
	 '(0 3 2 2 1 0)))
  (setf *tex*
	(gficl:make-texture-with-fn
	 10 10
	 #'(lambda (x y) (list (floor (* 255 (/ x 10))) (floor (* 255 (/ y 10))) 255 255))))
  (setf *bg-tex*
	(gficl:make-texture-with-fn
	 1000 1000
	 #'(lambda (x y)
	     (list (floor (* 200 (abs (sin (* x 0.002))))) (floor (* 200 (abs (cos (* y 0.002)))))
		   200 255))))
  
  (setf *model* (gficl:make-matrix))
  (setf *rot* 0))

(defun resize (w h)
  (if *fb* (gficl:delete-gl *fb*))
  (setf *fb* (gficl:make-framebuffer *fb-attachments* w h :samples *samples*))
  (setf *bg-model* (gficl:scale-matrix (list w h 1)))
  (setf *projection* (gficl:screen-orthographic-matrix
    (gficl:window-width) (gficl:window-height))))

(defun cleanup ()
  (gficl:delete-gl *tex*)
  (gficl:delete-gl *bg-tex*)
  (gficl:delete-gl *shader*)
  (if *fb* (gficl:delete-gl *fb*))
  (gficl:delete-gl *quad*))

(defun render ()
  (gficl:with-render   
   (gficl:bind-gl *fb*)
   (gl:clear :color-buffer :depth-buffer)
   
    (gficl:bind-gl *shader*)
    (gl:active-texture :texture0)
    (gficl:bind-matrix *shader* "projection" *projection*)
    (gficl:bind-matrix *shader* "model" *model*)
    (gficl:bind-gl *tex*)
    (gficl:draw-vertex-data *quad*)
    (gficl:bind-matrix *shader* "model" *bg-model*)
    (gficl:bind-gl *bg-tex*)
    (gficl:draw-vertex-data *quad*)

    (gficl:blit-framebuffers *fb* 0 (gficl:window-width) (gficl:window-height))))

(defun update ()
  (gficl:with-update (dt)
    (if (gficl:key-pressed :escape) (glfw:set-window-should-close))		     
    (if (gficl:key-pressed :f) (gficl:toggle-fullscreen))
    (setf *rot* (+ *rot* (* dt 1)))
    (setf *model*
	  (let* ((w (gficl:window-width))
		 (h (gficl:window-height))
		 (size (* 0.7 (min w h)))
		 (half (/ size 2)))
	    (gficl:*mat
	     (gficl:translation-matrix (list (- (/ w 2) half) (- (/ h 2) half) 0.1))
	     (gficl:translation-matrix (list half half 0))
	     (gficl:2d-rotation-matrix *rot*)
	     (gficl:translation-matrix (list (- half) (- half) 0))
	     (gficl:scale-matrix (list size size 1)))))))

(defun run ()
  (gficl:with-window
   (:title "spinning quad" :width 500 :height 500 :resize-callback #'resize)
   (setup)
    (loop until (gficl:closedp)
	  do (update)
	  do (render))
    (cleanup)))
