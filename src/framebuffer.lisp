(in-package :gficl)

;; --- framebuffer attachment ---

(defun color-attachment-p (position)
  (let ((n
	 (handler-case
	     (parse-integer
	      (handler-case 
		  (subseq (string position) 16)
		(error () "-1")))
	   (error () -1)))
	(max-attachments
	 (cffi:with-foreign-object (p :int)
	   ;; opengl does not change the supplied pointer
	   ;; to get-xxx functions if it hasn't been loaded yet
	   ;; so have some sane default for type checking before
	   ;; ogl is loaded			   
           (setf (cffi:mem-aref p :int) 16)
	   (%gl:get-integer-v :max-color-attachments p)
	   (cffi:mem-aref p :int))))
    (and (>= n 0)
	 (<= n max-attachments))))

(deftype attachment-position ()
	 '(or
	   (member
	    :depth-stencil-attachment
	    :depth-attachment
	    :stencil-attachment)
	   (satisfies color-attachment-p)))

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

(defmethod print-object ((obj attachment-description) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~a ~a"
	   (attachment-position obj) (attachment-type obj))))

;; --- framebuffer ---

(deftype draw-buffer ()
	 '(or (member :none :front-left :front-right :back-left :back-right)
	      (satisfies color-attachment-p)))

(defclass framebuffer (gl-object)
  ((attachments :initarg :attachments :accessor attachments)))

(declaim (ftype (function (list integer integer &key (:samples integer) (:draw-buffers list))
			  (values framebuffer &optional))
		make-framebuffer))
(defun make-framebuffer (attachments width height
				     &key (samples 1) (draw-buffers () draw-buffers-supplied))
  "creates a framebuffer from a list of attachment descriptions.
:draw-buffers is a list of DRAW-BUFFER items.
If :draw-buffers is not supplied, draw buffers will be all of the passed colour attachments."
  (let ((id (gl:gen-framebuffer))
	(draw-buffer-list draw-buffers)
	(internal-attachments
	 (loop for desc in attachments collecting
	       (progn (assert (typep desc 'attachment-description) (desc)
			      "~a was not an ATTACHMENT-DESCRIPTION" desc)
		      (make-attachment (attachment-position desc)
				       (attachment-type desc)
				       width height samples)))))
    (gl:bind-framebuffer :framebuffer id)
    (setf draw-buffer-list (loop for attachment in internal-attachments do
			     (attach-to-framebuffer attachment)
			     when (and (not draw-buffers-supplied)
				       (color-attachment-p (attachment-position attachment)))
			     collect (attachment-position attachment)))
    (case draw-buffer-list
	  (t (loop for e in draw-buffer-list do (assert (typep e 'draw-buffer)))
	     (gl:draw-buffers draw-buffer-list)))
    (let ((status (gl:check-framebuffer-status :framebuffer)))
      (unless (gl::enum= status :framebuffer-complete)
	(error "Failed to create framebuffer, gl error: ~a" status)))
    (create-gl)
    (make-instance 'framebuffer :attachments internal-attachments :id id)))

(defmethod delete-gl ((obj framebuffer))
  (loop for a in (attachments obj) do (delete-gl a))
  (gl:delete-framebuffers (list (id obj)))
  (call-next-method))

(defmethod bind-gl ((obj framebuffer))
  (gl:bind-framebuffer :framebuffer (id obj)))

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
			     (:buffer-list list) (:filter texture-filter)))
		blit-framebuffers))	
(defun blit-framebuffers (read-fb draw-fb width height &key	
				  (buffer-list (list :color-buffer-bit)) (filter :nearest))
  "blit framebuffers with same width and height, pass 0 or nil for backbuffer"
  (loop for b in buffer-list do
	(assert (typep b 'buffer-mask) ()
		"element ~a of buffer list is not a valid buffer mask: ~a"
		b '(:colour-buffer-bit :depth-buffer-bit :stencil-buffer-bit)))
  (let ((read (if (or (equalp read-fb 0) (not read-fb)) 0
		(progn (assert (typep read-fb 'framebuffer) ()
			       "read-fb must be a framebuffer (or nil)")
		       (id read-fb))))
	(draw (if (or (equalp draw-fb 0) (not draw-fb)) 0
		(progn (assert (typep draw-fb 'framebuffer) ()
			       "draw-fb must be a framebuffer (or nil)")
		       (id draw-fb)))))
    (gl:bind-framebuffer :draw-framebuffer draw)
    (gl:bind-framebuffer :read-framebuffer read)
    (%gl:blit-framebuffer 0 0 width height
			  0 0 width height
			  buffer-list filter)))

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

(declaim (ftype (function (attachment-position attachment-type integer integer integer
					       &key (:internal-format t))
			  (values attachment &optional))
		make-attachment))
(defun make-attachment (position resource-type width height samples
				 &key (internal-format nil))
  "Create attachment resource. Either a texture or a renderbuffer.
A suitable image format will be selected automatically if :internal-format is NIL."
  (assert (and (> width 0) (> height 0) (> samples 0)))
  (let* ((colour-attachment (color-attachment-p position))
	 (attachment-type (if colour-attachment :color position))
	 (format (ecase attachment-type
			(:color :rgb)
			(:depth-stencil-attachment :depth-stencil)
			(:depth-attachment :depth-component)
			(:stencil-attachment :depth-stencil)))
	 (internal-format (if internal-format internal-format format))
	 (data-type (ecase attachment-type
			   (:color :unsigned-byte)
			   (:depth-stencil-attachment :unsigned-int-24-8)
			   (:depth-attachment :float)
			   (:stencil-attachment :unsigned-int-24-8)))
	 (res (ecase resource-type
		     (:texture
		      (make-texture width height
				    :format format
				    :internal-format internal-format
				    :samples samples
				    :wrap :clamp-to-border
				    :data-type data-type))
		     (:renderbuffer
		      (make-renderbuffer internal-format width height samples)))))
    (if (equalp resource-type :texture)
	(case attachment-type
	      ((:depth-stencil-attachment :depth-attachment :stencil-attachment)
	       ;; disable depth comparison - TODO make this optional
	       (gl:tex-parameter :texture-2d :texture-compare-mode :none))))
    (make-instance 'attachment :position position :res res :res-type resource-type)))

(declaim (ftype (function (attachment)) attach-to-framebuffer))
(defun attach-to-framebuffer (attachment)
  (bind-gl (resource attachment))
  (ecase (attachment-type attachment)	 
	 (:texture (gl:framebuffer-texture-2d
		    :framebuffer
		    (attachment-position attachment) 
		    (tex-type (resource attachment))
		    (id (resource attachment)) 0))
	 (:renderbuffer (gl:framebuffer-renderbuffer
			 :framebuffer (attachment-position attachment)
			 :renderbuffer (id (resource attachment))))))

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
