(in-package :gficl)


(deftype image-format () '(member :red :rg :rgb :rgba :depth24-stencil8))


;;; ----------  Texture -------------

(defclass texture (gl-object)
  ((tex-type :initarg :tex-type :accessor tex-type :type texture-type)))

(deftype texture-type ()
  '(memeber :texture-2d :texture-2d-multisample))
(deftype texture-wrap ()
  '(member :clamp-to-edge :clamp-to-border :mirrored-repeat :repeat :mirrored-clamp-to-edge))
(deftype texture-filter () '(member :nearest :linear))

(defun make-texture (width
		     height
		     &key
		     (format :rgba)
		     (samples 1)
		     (data (cffi:null-pointer))
		     (mipmapping nil)
		     (wrap :repeat)
		     (filter :nearest))
  (declare (image-format format) (integer width) (integer height) (integer samples)
	   (texture-wrap wrap) (texture-filter filter))
  "Make a texture, will be a multisample image if samples > 1 
(in this case the data argument is not used) .
Data is a pointer to unsigned bytes or unsigned byte array, one byte for each channel."
  (assert (and (> width 0) (> height 0) (> samples 0)) (width height samples)
	  "Width (~d) Height (~d) Samples (~d) must all be greater than 0" width height samples)
  (let ((id (gl:gen-texture))
	(type (if (> samples 1) :texture-2d-multisample :texture-2d)))
    (gl:bind-texture type id)
    (if (equal type :texture-2d-multisample)
	(%gl:tex-image-2d-multisample type samples format width height :false)
	(gl:tex-image-2d type 0 format width height 0 format :unsigned-byte data))
    (if mipmapping (gl:generate-mipmap id))
    (if (equal type :texture-2d)
	(progn (gl:tex-parameter type :texture-wrap-s wrap)
	       (gl:tex-parameter type :texture-wrap-t wrap)
	       (gl:tex-parameter type :texture-min-filter filter)
	       (gl:tex-parameter type :texture-mag-filter filter)))
    (gl:bind-texture type 0)
    (make-instance 'texture :id id :tex-type type)))

(defun make-texture-with-fn (width height fn)
  (let ((channels 4))
    (cffi:with-foreign-pointer (data (* width height channels))
      (loop for x from 0 to (- width 1) do
	    (loop for y from 0 to (- height 1) do
		  (let ((bytes (funcall fn x y)))
		    (assert (= channels (length bytes)) (bytes)
			    "~a did not have the correct number of bytes" bytes)
		    (loop for channel from 0 to (- channels 1)
			  for value in bytes do
			  (assert (and (integerp value) (>= value 0) (<= value 255))
				  (data) "~a was not a byte" data)
			  (setf (cffi:mem-aref
				 data :uchar
				 (+ (* y width channels) (* x channels)
				    channel))
				value)))))
			       (gficl::make-texture width height :data data))))

(defmethod delete-gl ((obj texture))
	   (gl:delete-texture (id obj)))

(defun bind-texture (tex)
  (gl:bind-texture (tex-type tex) (id tex)))


;;; --------- Renderbuffer -----------

(defclass renderbuffer (gl-object) ())

(declaim (ftype (function (image-format integer integer integer) renderbuffer) make-renderbuffer))
(defun make-renderbuffer (format width height samples)
  "Make a renderbuffer, multisampled if samples > 1"
  (assert (and (> width 0) (> height 0) (> samples 0)))
  (let ((id (gl:gen-renderbuffer)))
    (gl:bind-renderbuffer :renderbuffer id)
    (if (> 1 samples)
	(gl:renderbuffer-storage-multisample :renderbuffer samples format width height)
	(gl:renderbuffer-storage :renderbuffer format width height))
    (gl:bind-renderbuffer :renderbuffer 0)
    (make-instance 'renderbuffer :id id)))

(defmethod delete-gl ((obj renderbuffer))
  (gl:delete-framebuffers (list (id obj))))
