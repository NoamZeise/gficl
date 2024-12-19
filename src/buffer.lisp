(in-package :gficl)

(deftype buffer-usage ()
  '(member
    ;; static - The data store contents will be modified once and used many times
    :static-read :static-draw :static-copy
    ;; stream - The data store contents will be modified once and used at most a few times
    :stream-read :stream-draw :stream-copy
    ;; dynamic - The data store contents will be modified repeatedly and used many times
    :dynamic-read :dynamic-draw :dynamic-copy
    ;; read - read | draw - write | copy - read/write
    ))

(defclass storage-buffer (gl-object)
  ((buffer-size :initarg :buffer-size)))

(defmethod delete-gl ((obj storage-buffer))
  (gl:delete-buffers (list (id obj)))
  (call-next-method))

(defmethod bind-gl ((obj storage-buffer))
  (gl:bind-buffer :shader-storage-buffer (id obj)))

(declaim (ftype (function (buffer-usage integer
				   &key (:data cffi:foreign-pointer))
			  (values storage-buffer &optional))
		make-storage-buffer))
(defun make-storage-buffer (usage size &key (data (cffi:null-pointer)))
  (let ((id (gl:gen-buffer)))
    (gl:bind-buffer :shader-storage-buffer id)
    (%gl:buffer-data :shader-storage-buffer size data usage)
    (create-gl)
    (make-instance 'storage-buffer :id id :buffer-size size)))

(declaim (ftype (function (buffer-usage t integer array)
			  (values storage-buffer &optional))
		make-storage-buffer-from-array))
(defun make-storage-buffer-from-array (usage type size lisp-array)
  (cffi:with-foreign-array
   (data lisp-array `(:array ,type ,size))
   (make-storage-buffer usage (* size (cffi:foreign-type-size type)) :data data)))

(declaim (ftype (function (storage-buffer integer &key (:offset integer) (:size integer)))
		bind-storage-buffer))
(defun bind-storage-buffer (storage-buffer binding-index
					   &key (offset 0 offset-supplied) (size 0 size-supplied))
  "bind storage buffer to BINDING-INDEX. 
If size is 0, the whole rest of the buffer after offset is bound."
  (if (not (or offset-supplied size-supplied))
      (%gl:bind-buffer-base :shader-storage-buffer binding-index (id storage-buffer))
    (with-slots (buffer-size) storage-buffer
      (let ((bind-size size))
	(if (>= offset buffer-size)
	    (error "offset larger than buffer size"))
	(if (= size 0) (setf bind-size (- buffer-size offset)))
	(if (> (+ offset bind-size) buffer-size)
	    (error "offset + size exceeded buffer size"))
	(%gl:bind-buffer-range
	 :shader-storage-buffer binding-index (id storage-buffer) offset bind-size)))))
