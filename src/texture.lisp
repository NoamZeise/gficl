(in-package :gficl)

(defclass texture (gl-object) ())

(deftype image-format () '(member :red :rg :rgb :rgba :depth24-stencil8))
(deftype texture-wrap ()
  '(member :clamp-to-edge :clamp-to-border :mirrored-repeat :repeat :mirrored-clamp-to-edge))
(deftype texture-filter () '(member :nearest :linear))

(defun make-texture (format width height
		     &key (samples 1) (data (cffi:null-pointer)) (mipmapping nil)
		       (wrap :repeat) (filter :nearest))
  (declare (image-format format) (integer width) (integer height) (integer samples)
	   (texture-wrap wrap) (texture-filter filter))
  "Make a texture, will be a multisample image if samples > 1.
Data is a pointer to unsigned bytes or unsigned byte array."
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
    (make-instance 'texture :id id)))

(defmethod delete-gl ((obj texture))
  (gl:delete-texture (id obj)))
