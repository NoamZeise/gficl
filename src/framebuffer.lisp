(in-package :gficl)

(defclass framebuffer (gl-object)
  ((attachments :initargs :attachments :accessor attachments)))

(defun create-framebuffer (attachments width height samples)
  (let ((id (gl:gen-framebuffer))
	(internal-attachments ())
	(draw-buffers ()))
    (gl:bind-framebuffer id)
    (setq internal-attachments
	  (loop for (pos type) in attachments
		collecting
		(let ((res (make-attachment pos type width height samples)))
		  (ecase type
		    (:texture
		     (progn (gl:framebuffer-texture-2d :framebuffer pos (tex-type res) (id res) 0)
			    ;; texture attachments will 
			    (nconc draw-buffers (cons pos nil))))
		    (:renderbuffer
		     (gl:framebuffer-renderbuffer :framebuffer pos :renderbuffer (id res))))
		  res)))
    (if draw-buffers (gl:draw-buffers draw-buffers))
    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= status :framebuffer-complete)
	(error "Failed to create framebuffer, gl error: ~A" status)))
    (gl:bind-framebuffer 0)
    (make-instance 'framebuffer :attachments internal-attachments :id id)))

(defmethod delete-gl ((obj framebuffer))
  (for a in (attachments obj) do (delete-gl a))
  (gl:delete-framebuffers (id obj)))

(defun framebuffer-attach-id (framebuffer index))

(deftype attachment-position ()
  '(member  :color-attachment0 :depth-stencil-attachment))

(deftype attachment-resource () '(member :texture :renderbuffer))

(defclass attachment ()
  ((attach-pos :initarg :position :accessor attach-pos :type attachment-position)
   (res-type :initarg :res-type :accessor res-type :type attachment-resource)
   (resource :initarg :res :accessor resource)))

(defun make-attachment (position resource-type width height samples)
  (declare (attachment-position position) (attachment-resource resource-type)
	   (integer width) (integer height) (integer samples))
  "Create attachment resource. Either a texture or a renderbuffer."
  (assert (and (> 0 width) (> 0 height) (> 0 samples)))
  (let* ((format (ecase position
		   (:color-attachment0 :rgb)
		   (:depth-stencil-attachment :depth24-stencil8)))
	 (res (ecase resource-type
		(:texture
		 (make-texture format width height :samples samples :wrap :clamp-to-border))
		(:renderbuffer
		 (make-renderbuffer format width height samples)))))
    (make-instance 'attachment :position position :res-type resource-type :resource res)))

(defmethod delete-gl ((obj attachment))
  (delete-gl (resource obj)))
