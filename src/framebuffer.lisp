(in-package :gficl)

;; --- framebuffer attachment ---

(defun color-attachment-p (position)
  "Returns T if position is a valid framebuffer colour attachment position, 
returns NIL otherwise."
  (let ((n
	 (handler-case
	     (parse-integer
	      (handler-case 
		  (subseq (string position) 16)
		(error () "-1")))
	   (error () -1)))
	(max-attachments
	 (cffi:with-foreign-object (p :int)
				   ;; opengl does not change the supplied pointer to
				   ;; gl:get-xxx functions if it hasn't been loaded yet
				   ;; so we use some sane default for type checking
				   ;; that happens before ogl is loaded
				   (setf (cffi:mem-aref p :int) 16)
				   (handler-case (%gl:get-integer-v :max-color-attachments p)
				     (error ()))
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

(defstruct (attachment-description (:conc-name attach-desc-))
	   "Defines the properties of a FRAMEBUFFER attachment.
A list of ATTACHMENT-DESCRIPTION is passed to MAKE-FRAMEBUFFER.

ATTACHMENT-POSITION is the type of rendering the attachment will be used for.
It must be one of:
- :color-attachmentN (where N is a positive integer)
- :depth-attachment
- :stencil-attachment
- :depth-stencil-attachment

:type can be a :renderbuffer or a :texture
- :renderbuffer for attachments you do not need to sample from
- :texture for attachments you want to sample from
  texture ids can be accessed via FRAMEBUFFER-TEXTURE-ID

:wrap will only affect framebuffers of type :texture and determine how
it is sampled outside of it's range.

:interal-format is the internal texture format used by the texture or renderbuffer"
	   (position :color-attachment0 :type attachment-position)
	   (type :renderbuffer :type attachment-type)
	   (wrap :clamp-to-edge :type texture-wrap)
	   (internal-format nil))

(defmethod print-object ((obj attachment-description) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~a ~a"
	   (attach-desc-position obj) (attach-desc-type obj))))

(declaim (ftype (function (attachment-description) t) attach-desc-clear-bit))
(defun attach-desc-clear-bits (attach-desc)
  "Return a list of clear bits that would completely clear the attachment."
  (with-slots (position) attach-desc
    (cond ((color-attachment-p position)
	   (list :color-buffer-bit))
	  ((eql position :depth-attachment)
	   (list :depth-buffer-bit))
	  ((eql position :stencil-attachment)
	   (list :stencil-buffer-bit))
	  ((eql position :depth-stencil-attachment)
	   (list :depth-buffer-bit :stencil-buffer-bit)))))

;; --- framebuffer ---

(deftype draw-buffer ()
	 '(or (member :none :front-left :front-right :back-left :back-right)
	      (satisfies color-attachment-p)))

(defclass framebuffer (gl-object)
  ((attachments :initarg :attachments :accessor attachments)))

(declaim (ftype (function (list integer integer &key (:samples integer) (:draw-buffers list))
			  (values framebuffer &optional))
		make-framebuffer))
(defun make-framebuffer (attachment-descriptions width height
				     &key (samples 1) (draw-buffers () draw-buffers-supplied))
  "creates a framebuffer from a list of attachment descriptions. 
Must be manually freed with DELETE-GL.

The index of a framebuffer attachment is equal to it's position in the ATTACHMENT-DESCRIPTIONS list.

:draw-buffers is a list of DRAW-BUFFER items. 
If :draw-buffers is not supplied, draw buffers will be all of the passed colour attachments.
"
  (let ((id (gl:gen-framebuffer))
	(draw-buffer-list draw-buffers)
	(internal-attachments
	 (loop for desc in attachment-descriptions collecting
	       (progn (assert (typep desc 'attachment-description) (desc)
			      "~a was not an ATTACHMENT-DESCRIPTION" desc)
		      (make-attachment desc width height samples)))))
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
  "Return the texture id of the framebuffer attachment at index INDEX.
Will signal an error if the index is out of range, or if the attachment at that index is not a texture attachment."
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
  "blit framebuffers with same width and height, such as for resolving a multisample framebuffer.
Pass 0 or nil as READ-FB or DRAW-FB to use the backbuffer as the blit source or destination respectively."
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

(declaim (ftype (function (attachment-description integer integer integer)
			  (values attachment &optional))
		make-attachment))
(defun make-attachment (desc width height samples)
  "Create attachment resource. Either a texture or a renderbuffer.
A suitable image format will be selected based on attachment position."
  (assert (and (> width 0) (> height 0) (> samples 0)))
  (let* ((pos (attach-desc-position desc))
	 (resource-type (attach-desc-type desc))
	 (colour-attachment (color-attachment-p pos))
	 (attachment-type (if colour-attachment :color pos))
	 (format (ecase attachment-type
			(:color :rgb)			
			(:depth-attachment :depth-component)
			((:depth-stencil-attachment :stencil-attachment) :depth-stencil)))
	 (internal-format (if (attach-desc-internal-format desc)
			      (attach-desc-internal-format desc) format))
	 (data-type (ecase attachment-type
			   (:color :unsigned-byte)
			   (:depth-stencil-attachment :unsigned-int-24-8)
			   (:depth-attachment :float)
			   (:stencil-attachment :unsigned-int-24-8)))
	 (filter (case attachment-type
		       ((:depth-stencil-attachment :depth-attachment)
			;; depth comparison - TODO make this optional
			:linear)
		       (otherwise :nearest)))
	 (res (ecase resource-type
		     (:texture
		      (make-texture width height
				    :format format
				    :internal-format internal-format
				    :samples samples
				    :wrap (attach-desc-wrap desc)
				    :data-type data-type
				    :filter filter
				    :mipmapping nil))
		     (:renderbuffer
		      (make-renderbuffer internal-format width height samples)))))
    (make-instance 'attachment :position pos :res res :res-type resource-type)))

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
   (format out "~a ~a ~a"
	   (attachment-position obj)
	   (attachment-type obj)
	   (resource obj))))
