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

(declaim (ftype (function (integer cffi:foreign-pointer buffer-usage)
			  (values storage-buffer &optional))
		make-storage-buffer))
(defun make-storage-buffer (size data usage)
  (let ((id (gl:gen-buffer)))
    (gl:bind-buffer :shader-storage-buffer id)
    (%gl:buffer-data :shader-storage-buffer size data usage)
    (create-gl)
    (make-instance 'storage-buffer :id id :buffer-size size)))

(declaim (ftype (function (integer buffer-usage)
			  (values storage-buffer &optional))
		make-empty-storage-buffer))
(defun make-empty-storage-buffer (size usage)
  "create an uninitialized shader storage buffer of SIZE bytes"
  (make-storage-buffer size (cffi:null-pointer) usage))

(declaim (ftype (function (integer t buffer-usage array)
			  (values storage-buffer &optional))
		make-storage-buffer-from-array))
(defun make-storage-buffer-from-array (size type usage lisp-array)
  (cffi:with-foreign-array
   (data lisp-array `(:array ,type ,size))
   (make-storage-buffer (* size (cffi:foreign-type-size type)) data usage)))

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
