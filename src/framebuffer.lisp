(in-package :gficl)

(defclass framebuffer (gl-object)
  ((attachments :initarg :attachments :accessor attachments)))

(defun create-framebuffer (attachments width height samples)
  (declare (integer width) (integer height) (integer samples))
  (let ((id (gl:gen-framebuffer))
	(internal-attachments ())
	(draw-buffers ()))
    (gl:bind-framebuffer :framebuffer id)
    (setq internal-attachments
	  (loop for (pos type) in attachments
		collecting
		(let ((res (make-attachment pos type width height samples)))
		  (ecase type
		    (:texture
		     (progn
		       (gl:framebuffer-texture-2d :framebuffer pos
						  (tex-type (resource res))
						  (id (resource res)) 0)
		       (nconc draw-buffers (cons pos nil))))
		    (:renderbuffer
		     (gl:framebuffer-renderbuffer :framebuffer pos :renderbuffer
						  (id (resource res)))))
		  res)))
    (if draw-buffers (gl:draw-buffers draw-buffers))
    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= status :framebuffer-complete)
	(error "Failed to create framebuffer, gl error: ~A" status)))
    (gl:bind-framebuffer :framebuffer 0)
    (make-instance 'framebuffer :attachments internal-attachments :id id)))

(defmethod delete-gl ((obj framebuffer))
  (loop for a in (attachments obj) do (delete-gl a))
  (gl:delete-framebuffers (list (id obj))))

(declaim (ftype (function (framebuffer integer) integer) framebuffer-attach-id))
(defun framebuffer-attach-id (framebuffer index)
  (let ((attach (attachments framebuffer)))
    (dotimes (i index)
      (if (or (equalp attach nil) (equalp (cdr attach) nil))
	  (error "Framebuffer attachment index was greater than the number of attachments"))
      (setf attach (cdr attach)))
    (if (equalp (res-type (car attach)) :texture)
	(id (resource (car attach)))
 	(error "Tried to get the attachment id of a non texture attachment"))))

(deftype attachment-position ()
  '(member  :color-attachment0 :depth-stencil-attachment))

(deftype attachment-resource () '(member :texture :renderbuffer))

(defclass attachment ()
  ((attach-pos :initarg :position :accessor attach-pos :type attachment-position)
   (res-type :initarg :res-type :accessor res-type :type attachment-resource)
   (resource :initarg :res :accessor resource)))

(declaim (ftype (function (attachment-position attachment-resource integer integer integer)
			  attachment)
		make-attachment))
(defun make-attachment (position resource-type width height samples)
  "Create attachment resource. Either a texture or a renderbuffer."
  (assert (and (> width 0) (> height 0) (> samples 0)))
  (let* ((format (ecase position
		   (:color-attachment0 :rgb)
		   (:depth-stencil-attachment :depth24-stencil8)))
	 (res (ecase resource-type
		(:texture
		 (make-texture format width height :samples samples :wrap :clamp-to-border))
		(:renderbuffer
		 (make-renderbuffer format width height samples)))))
    (make-instance 'attachment :position position :res res :res-type resource-type)))

(defmethod delete-gl ((obj attachment))
  (delete-gl (resource obj)))
