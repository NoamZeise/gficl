(in-package :gficl)

;; --- framebuffer attachment ---

(deftype attachment-position ()
	 '(member :color-attachment0 :depth-stencil-attachment))

(deftype attachment-type () '(member :texture :renderbuffer))

(defclass attachment-description ()
  ((pos :initarg :pos :accessor attachment-position :type attachment-position)
   (type :initarg :type :accessor attachment-type :type attachment-type)))

(declaim (ftype (function (attachment-position &optional attachment-type)
			  (values attachment-description &optional))
		make-attachment-description))
(defun make-attachment-description (position &optional (type :renderbuffer))
  "defines the form an attachment to a framebuffer should take"
  (make-instance 'attachment-description :pos position :type type))

;; --- framebuffer ---

(defclass framebuffer (gl-object)
  ((attachments :initarg :attachments :accessor attachments)))

(declaim (ftype (function (list integer integer &optional integer) (values framebuffer &optional))
		make-framebuffer))
(defun make-framebuffer (attachments width height &optional (samples 1))
  "creates a framebuffer from a list of attachment descriptions"
  (let ((id (gl:gen-framebuffer))
	(draw-buffers ())
	(internal-attachments
	 (loop for desc in attachments collecting
	       (progn (assert (typep desc 'attachment-description) (desc)
			      "~a was not an ATTACHMENT-DESCRIPTION" desc)
		      (make-attachment (attachment-position desc)
				       (attachment-type desc)
				       width height samples)))))
    (gl:bind-framebuffer :framebuffer id)
    (setf draw-buffers (loop for attachment in internal-attachments do
			     (attach-to-framebuffer attachment)
			     when (equalp (attachment-type attachment) :texture)
			     collect (attachment-position attachment)))
    (if draw-buffers (gl:draw-buffers draw-buffers))
    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= status :framebuffer-complete)
	(error "Failed to create framebuffer, gl error: ~a" status)))
    (make-instance 'framebuffer :attachments internal-attachments :id id)))

(defmethod delete-gl ((obj framebuffer))
  (loop for a in (attachments obj) do (delete-gl a))
  (gl:delete-framebuffers (list (id obj))))

(defmethod bind-gl ((obj framebuffer))
  (gl:bind-framebuffer :framebuffer (id framebuffer)))

(declaim (ftype (function (framebuffer integer) (values integer &optional)) framebuffer-attach-id))
(defun framebuffer-texture-id (framebuffer index)
  (let ((attach (attachments framebuffer)))
    (dotimes (i index)
      (if (or (equalp attach nil) (equalp (cdr attach) nil))
	  (error "Framebuffer attachment index was greater than the number of attachments"))
      (setf attach (cdr attach)))
    (if (equalp (attachment-type (car attach)) :texture)
	(id (resource (car attach)))
 	(error "Tried to get the attachment id of a non texture attachment"))))

(declaim (ftype (function (t t integer integer &key
			     (:buffer buffer-mask) (:filter texture-filter)))))
(defun blit-framebuffers (read-fb draw-fb width height &key
				  (buffer :color-buffer-bit) (filter :nearest))
  "blit framebuffers with same width and height, pass nil for backbuffer"
  (let ((read (if (not read-fb) 0
		(progn (id read-fb)
		       (assert (typep read-fb 'framebuffer) ()
			       "read-fb must be a framebuffer or nil"))))
	(draw (if (not draw-fb) 0
		(progn (id draw-fb)
		       (assert (typep draw-fb 'framebuffer) ()
			       "draw-fb must be a framebuffer or nil")))))
    (gl:bind-framebuffer :draw-framebuffer draw)
    (gl:bind-framebuffer :read-framebuffer read)
    (%gl:blit-framebuffer 0 0 width height
			  0 0 width height
			  buffer filter)))

(defmethod print-object ((obj framebuffer) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~{~%  ~a~}"
	   (attachments obj))))

;; --- internal attachment ---

(defclass attachment ()
  ((pos :initarg :position :accessor attachment-position :type attachment-position)
   (type :initarg :res-type :accessor attachment-type :type attachment-type)
   (resource :initarg :res :accessor resource :type gl-object)))

(declaim (ftype (function (attachment-position attachment-type integer integer integer)
			  (values attachment &optional))
		make-attachment))
(defun make-attachment (position resource-type width height samples)
  "Create attachment resource. Either a texture or a renderbuffer."
  (assert (and (> width 0) (> height 0) (> samples 0)))
  (let* ((format (ecase position
		   (:color-attachment0 :rgb)
		   (:depth-stencil-attachment :depth24-stencil8)))
	 (res (ecase resource-type
		(:texture
		 (make-texture width height :format format :samples samples :wrap :clamp-to-border))
		(:renderbuffer
		 (make-renderbuffer format width height samples)))))    
    (make-instance 'attachment :position position :res res :res-type resource-type)))

(declaim (ftype (function (attachment)) attach-to-framebuffer))
(defun attach-to-framebuffer (attachment)
  (bind-gl (resource attachment))
  (ecase (attachment-type attachment)	 
	 (:texture (gl:framebuffer-texture-2d :framebuffer
					      (attachment-position attachment) 
					      (tex-type (resource attachment))
					      (id (resource attachment)) 0))
	 (:renderbuffer (gl:framebuffer-renderbuffer :framebuffer
						     (attachment-position attachment)
						     :renderbuffer
						     (id (resource attachment))))))

(defmethod id ((obj attachment))
  (id (resource obj)))

(defmethod delete-gl ((obj attachment))
  (delete-gl (resource obj)))

(defmethod bind-gl ((obj attachment))
  (bind-gl (resource obj)))

(defmethod print-object ((obj attachment) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~a ~a"
	   (attach-pos obj)
	   (resource obj))))
