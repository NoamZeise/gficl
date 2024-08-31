(in-package :gficl-examples/font)

(defparameter *tex* nil)
(defparameter *quad* nil)
(defparameter *shader* nil)

(defparameter *vert-shader*
	      "#version 330
layout (location = 0) in vec2 vertex;

out vec2 TexCoords;

uniform mat4 model;
uniform mat4 projection;

void main() {
    TexCoords = vertex;
    gl_Position = projection * model * vec4(vertex, 0, 1);
}")
(defparameter *frag-shader*
  "#version 330

in vec2 TexCoords;
out vec4 colour;

uniform sampler2D tex;

void main() {
  float text_alpha = texture(tex, TexCoords).r;
  if(text_alpha == 0)
    discard;
  vec3 text_colour = vec3(1);
  colour = vec4(text_colour, text_alpha);
}")

(defparameter *font-path* #p"examples/assets/Roboto-Regular.ttf")
(defparameter *character-range* '(32 126))
(defparameter *font-size* 100)
(defparameter *font-dpi* 500)
(defparameter *font-ht* nil)

(defclass character-info ()
  ((data :initarg :data)
   (tex-offset :initarg :offset :type gficl:vec)
   (position :initarg :size :type gficl:vec)
   (advance :initarg :bearing :type gficl:vec)))

(defun setup ()
  (gl:clear-color 0 1 0 0)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (setf *shader* (gficl:make-shader *vert-shader* *frag-shader*))
  (setf *font-ht* nil)
  (let ((tex-width 0) (tex-height 0))
    (loop for i from (car *character-range*) to (cadr *character-range*) do
	  (multiple-value-bind
	   (data min-x max-y width height)
	   (truetype-clx:text-pixarray *font-path* (string (code-char i))
				       *font-size* *font-dpi* *font-dpi*)
	   (format t "character : ~a is ~a , ~a x ~a , ~a    ~a~%" (code-char i)
		   min-x width max-y height (if data (array-dimensions data) nil)))))
  
  (loop for i from 65 to 65 do	
	(let ((text (truetype-clx:text-pixarray *font-path* (string (code-char i)) 100 600 600)))
	  (if text
	      (destructuring-bind (h w) (array-dimensions text)
				  (let ((data (cffi:foreign-alloc
					       :unsigned-char :count (* w h))))
				    (loop for x from 0 below w do
					  (loop for y from 0 below h do
						(setf (cffi:mem-aref data :unsigned-char
								     (+ (* y w) x))
						      (aref text y x))))
				    (setf *tex*
					  (gficl:make-texture w h :data data
							      :wrap :clamp-to-edge
							      :format :red))
				    (cffi:foreign-free data))))))
  (setf *quad* (gficl:make-vertex-data
		(gficl:make-vertex-form
		 (list (gficl:make-vertex-slot 2 :float)))
		'(((0 0)) ((1 0)) ((1 1)) ((0 1))) '(0 3 2 2 1 0)))
  (resize (gficl:window-width) (gficl:window-height))
  (gficl:bind-matrix *shader* "model"
		     (gficl:*mat
		      (gficl:translation-matrix '(50 50 0))
		      (gficl:scale-matrix '(300 300 1)))))

(defun resize (w h)
  (gficl:bind-gl *shader*)
   (gficl:bind-matrix *shader* "projection"
		      (gficl:screen-orthographic-matrix w h)))

(defun cleanup ()
  (gficl:delete-gl *shader*)
  (gficl:delete-gl *tex*)
  (gficl:delete-gl *quad*))

(defun update ()
  (gficl:with-update ()
    (gficl:map-keys-down (:escape (glfw:set-window-should-close)))))

(defun render ()
  (gficl:with-render
   (gl:clear :color-buffer)
   (gficl:bind-gl *shader*)
   (gl:active-texture :texture0)
   (gficl:bind-gl *tex*)
   (gficl:draw-vertex-data *quad*)))

(defun run ()
  (gficl:with-window
   (:title "font rendering" :width 600 :height 400 :resize-callback 'resize)
   (setup)
   (loop until (gficl:closed-p)
	 do (update)
	 do (render))
   (cleanup)))
