(in-package :gficl)

(deftype colour-image-format () '(member :red :rg :rgb :rgba ))
(deftype image-format ()
	 '(or colour-image-format
	      (member :depth-component :depth-stencil)))
(deftype internal-image-format ()
	 '(or image-format
	      (member :depth24-stencil8
		      :depth32f-stencil8
		      :depth-component16
		      :depth-component24
		      :depth-component32f)))

(declaim (ftype (function (integer) image-format) get-image-format))
(defun get-image-format (channels)
  "return image format matching passed number of channels (1-4)"
  (ecase channels
	 (1 :red)
	 (2 :rg)
	 (3 :rgb)
	 (4 :rgba)))

;;; ----------  Texture -------------

(deftype texture-type ()
	 '(memeber :texture-2d :texture-2d-multisample))

(deftype texture-wrap ()
  '(member :clamp-to-edge :clamp-to-border :mirrored-repeat :repeat :mirrored-clamp-to-edge))

(deftype texture-filter () '(member :nearest :linear))

(defclass texture (gl-object)
  ((tex-type :initarg :tex-type :accessor tex-type :type texture-type)
   (samples :initarg :samples :accessor tex-samples :type integer)))

(declaim (ftype (function (integer integer &key
				   (:format image-format)
				   (:internal-format t)
				   (:samples integer)
				   (:data cffi:foreign-pointer)
				   (:mipmapping boolean)
				   (:wrap texture-wrap)
				   (:filter texture-filter)
				   (:data-type t))
			  (values texture &optional))
		make-texture))
(defun make-texture
    (width height &key
	   (format :rgba)
	   (internal-format nil)
	   (samples 1)
	   (data (cffi:null-pointer))
	   (mipmapping t)
	   (wrap :repeat)
	   (filter :nearest)
	   (data-type :unsigned-byte))
  (declare (integer width) (integer height) (integer samples))
  "Make a texture, must be freed with DELETE-GL.

Data should be null or a pointer to texture data
with enough bytes to create the texture, one byte for each channel.
(no. bytes = width * height * channels)

If samples > 1, a multisample texture will be created, 
in this case the data argument is ignored.

If internal format is nil, it will be the same as format.
This will work for colour textures, 
but internal format must be speicifed for depth-stencil textures"
  (assert (and (> width 0) (> height 0) (> samples 0)) (width height samples)
	  "Width (~d) Height (~d) Samples (~d) must all be greater than 0"
	  width height samples)
  (if internal-format nil ;;(assert (typep internal-format 'internal-image-format))
    (if (typep format 'colour-image-format)
	(setf internal-format format)
      (error "Missing :internal-format
When making non-colour textures, :format and :internal-format must be supplied.")))
  (let ((id (gl:gen-texture))
	(type (if (> samples 1) :texture-2d-multisample :texture-2d)))
    (gl:bind-texture type id)
    (if (or (equalp format :red) (equalp format :rgb))
	(gl:pixel-store :unpack-alignment 1))
    (if (equal type :texture-2d-multisample)
	(%gl:tex-image-2d-multisample type samples internal-format width height :false)
      (gl:tex-image-2d type 0 internal-format width height 0 format data-type data))
    (if mipmapping (gl:generate-mipmap type))
    (if (equal type :texture-2d)
	(progn (gl:tex-parameter type :texture-wrap-s wrap)
	       (gl:tex-parameter type :texture-wrap-t wrap)
	       (gl:tex-parameter type :texture-min-filter filter)
	       (gl:tex-parameter type :texture-mag-filter filter)))
    (create-gl)
    (make-instance 'texture :id id :tex-type type :samples samples)))

(declaim (ftype (function (integer
			   integer
			   (function (integer integer) list)
			   &key
			   (:channels integer)
			   (:mipmapping boolean)
			   (:wrap texture-wrap)
			   (:filter texture-filter))
			  (values texture &optional))
		make-texture-with-fn))
(defun make-texture-with-fn (width height fn &key
				   (channels 4) (mipmapping t) (wrap :repeat) (filter :nearest))
  "FN is called for each pixel with x y args and must return CHANNELS number of bytes"
  (declare (integer channels))
  (assert (and (>= channels 1) (<= channels 4))
	  (channels)
	  "channels was ~a,  must be between 1 and 4"
	  channels)
  (cffi:with-foreign-pointer (data (* width height channels))
    (loop for x from 0 to (- width 1) do
	  (loop for y from 0 to (- height 1) do
		(let ((bytes (funcall fn x y)))
		  (assert (= channels (length bytes)) (bytes)
			  "~a did not have the correct number of bytes" bytes)
		  (loop for channel from 0 to (- channels 1)
			for value in bytes do
			(assert (and (integerp value) (>= value 0) (<= value 255))
				(value) "~a was not a byte" value)
			(setf (cffi:mem-aref
			       data :uchar (+ (* y width channels) (* x channels) channel))
			      value)))))
    (gficl::make-texture width height :data data :format (get-image-format channels)
			 :mipmapping mipmapping :wrap wrap :filter filter)))

(defmethod delete-gl ((obj texture))
   (gl:delete-texture (id obj))
   (call-next-method))

(defmethod bind-gl ((obj texture))
  (gl:bind-texture (tex-type obj) (id obj)))

(defmethod print-object ((obj texture) out)
   (print-unreadable-object
    (obj out :type t)
    (format out "samples: ~a, id: ~a"
	    (tex-samples obj) (id obj))))

;;; --------- Renderbuffer -----------

(defclass renderbuffer (gl-object)
  ((samples :initarg :samples :accessor rb-samples)))

(declaim (ftype (function (t ;;internal-image-format
			   integer integer integer)
			  (values renderbuffer &optional))
		make-renderbuffer))
(defun make-renderbuffer (format width height samples)
  "Make a renderbuffer, multisampled if samples > 1.
Must be manually freed with DELETE-GL."
  (assert (and (> width 0) (> height 0) (> samples 0)))
  (let ((id (gl:gen-renderbuffer)))
    (gl:bind-renderbuffer :renderbuffer id)
    (if (> samples 1)
	(gl:renderbuffer-storage-multisample :renderbuffer samples format width height)
        (gl:renderbuffer-storage :renderbuffer format width height))
    (create-gl)
    (make-instance 'renderbuffer :id id :samples samples)))

(defmethod delete-gl ((obj renderbuffer))
  (gl:delete-renderbuffers (list (id obj)))
  (call-next-method))

(defmethod bind-gl ((obj renderbuffer))
  (gl:bind-renderbuffer :renderbuffer (id obj)))

(defmethod print-object ((obj renderbuffer) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "samples: ~a, id: ~a"
	   (rb-samples obj) (id obj))))
