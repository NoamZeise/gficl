(in-package :gficl)



;;; ----- vertex type spec -----

(deftype vertex-elem-type ()
  '(member
    :float :half-float :double :fixed
    :int :unsigned-int
    :byte :unsigned-byte
    :short :unsigned-short))

(defclass vertex-slot ()
  ((vector-size :initarg :vector-size :accessor vector-size :type integer)
   (element-type :initarg :element-type :accessor element-type :type vertex-elem-type)))

(declaim (ftype (function (integer vertex-elem-type) vertex-slot) make-vertex-slot))
(defun make-vertex-slot (vector-size element-type)
  (make-instance 'vertex-slot :vector-size vector-size :element-type element-type))

(defclass vertex-form ()
  ((vertex-slots :initarg :vertex-slots :accessor vertex-slots)
   (slot-offsets :initarg :slot-offsets :accessor slot-offsets)
   (vertex-size :initarg :vertex-size :accessor vertex-size)))

(defun make-vertex-form (vertex-slots)
  (let* ((slot-offsets (list))
	 (vertex-size 0))
    (loop for slot in vertex-slots
	  do (progn
	       (assert (typep slot 'vertex-slot))
	       (push vertex-size slot-offsets)
	       (let ((slot-size
		       (* (vector-size slot) (cffi:foreign-type-size (element-type slot)))))
		 (setf vertex-size (+ vertex-size slot-size)))))
    (make-instance 'vertex-form :vertex-slots vertex-slots
				:slot-offsets (nreverse slot-offsets)
				:vertex-size vertex-size)))

(defparameter *3d-vertex*
  (make-vertex-form
   (list
    (make-vertex-slot 3 :float)	   ; position
    (make-vertex-slot 3 :float)	   ; normal
    (make-vertex-slot 2 :float)))) ; tex coords



;;; ----- OpenGL vertex arrays -----

(defclass vertex-data ()
  ((vao :initarg :vao :accessor vao)
   (vbo :initarg :vbo :accessor vbo)
   (ebo :initarg :ebo :accessor ebo)
   (index-count :initarg :index-count :accessor index-count)
   (draw-mode :initform :triangles :accessor draw-mode)))

(defmethod delete-gl ((obj vertex-data))
  (gl:delete-buffers '((vbo vertex-data) (ebo vertex-data)))
  (gl:delete-vertex-arrays '(vao vertex-data)))

(defun make-vertex-data (vertex-form vertices indices)
  (let* ((vao (gl:gen-vertex-array))
	 (buffers (gl:gen-buffers 2))
	 (vbo (elt buffers 0))
	 (ebo (elt buffers 1)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (gl:buffer-data :array-buffer :static-draw (vertex-list-to-array vertex-form vertices))
    (gl:bind-buffer :element-array-buffer ebo)
    (gl:buffer-data :element-array-buffer :static-draw (index-list-to-array indices))
    (setup-vertex-attrib-array vertex-form)
    (make-instance 'vertex-data :vao vao :vbo vbo :ebo ebo :index-count (length indices))))

(defun draw-vertex-data (vertex-data &key (verticies 0 vertp) (instances 1))
  (assert (and (> verticies 0) (> instances 0) (<= verticies (index-count vertex-data)))
	  (verticies instances)
	  "draw vertex data args were out of range. verticies (~d) instances (~d)"
	  verticies instances)
  (if (not vertp) (setf verticies (index-count vertex-data)))
  (gl:bind-vertex-array (vao vertex-data))
  (if (> instances 1)
      (%gl:draw-elements-instanced (draw-mode vertex-data) verticies :unsigned-int 0 instances)
      (%gl:draw-elements (draw-mode vertex-data) verticies :unsigned-int 0)))



;;; ----- Helpers -----

(defun vertex-list-to-array (vertex-form vertices)
  ;;todo: make this fn actually do something
  (assert (listp vertices))
  (let* ((vertex-count (length vertices))
	 (buff (cffi:foreign-alloc :char :count (* (vertex-size vertex-form) vertex-count))))
    ;; TODO: validate buffer has expected form
    ;;       fill buffer with vertex data
    (cffi:foreign-free buff)))

(defun index-list-to-array (indices)
  (assert (listp indices))
  (let* ((count (length indices))
	 (arr (gl:alloc-gl-array :int count)))
    (loop for i from 0 for index in indices do
      (assert (and (typep index 'integer) (>= 0 index)))
      (setf (gl:glaref arr i) index))
    arr))

(defun setup-vertex-attrib-array (vertex-form)
  (loop for i from 0
	for slot in (vertex-slots vertex-form)
	for offset in (slot-offsets vertex-form) do
	  (progn
	    (gl:enable-vertex-attrib-array i)
	    (gl:vertex-attrib-pointer
	     i (vector-size slot) (element-type slot) nil
	     (vertex-size vertex-form) (cffi:make-pointer offset)))))
