(in-package :gficl)

;;; ----- vertex type spec -----

(deftype vertex-elem-type ()
	 '(member
	   :float :double
	   :int :unsigned-int
	   :char :unsigned-char
	   :short :unsigned-short))

(defclass vertex-slot ()
  ((vector-size :initarg :vector-size :accessor vector-size :type integer)
   (element-type :initarg :element-type :accessor element-type :type vertex-elem-type)))

(declaim (ftype (function (integer vertex-elem-type) vertex-slot) make-vertex-slot))
(defun make-vertex-slot (vector-size element-type)
  "define an element of a vertex in a shader. 
ie position, normal, texcoords, etc."
  (make-instance 'vertex-slot :vector-size vector-size :element-type element-type))

(defclass vertex-form ()
  ((vertex-slots :initarg :vertex-slots :accessor vertex-slots)
   (slot-offsets :initarg :slot-offsets :accessor slot-offsets)
   (vertex-mem-size :initarg :vertex-mem-size :accessor vertex-mem-size)))

(defun make-vertex-form (vertex-slots)
  "define a vertex shader input based on the supplied vertex slots.
The order must match the vertex locations in the shader"
  (let* ((slot-offsets (list))
	 (vertex-mem-size 0))
    (loop for slot in vertex-slots
	  do (progn
	       (assert (typep slot 'vertex-slot))
	       (push vertex-mem-size slot-offsets)
	       (let ((slot-size
		      (* (vector-size slot) (cffi:foreign-type-size (element-type slot)))))
		 (setf vertex-mem-size (+ vertex-mem-size slot-size)))))
    (make-instance 'vertex-form :vertex-slots vertex-slots
		   :slot-offsets (nreverse slot-offsets)
		   :vertex-mem-size vertex-mem-size)))

(defparameter *3d-vertex*
  (make-vertex-form
   (list
    (make-vertex-slot 3 :float)	   ; position
    (make-vertex-slot 2 :float)))) ; tex coords

;;; ----- OpenGL vertex arrays -----

(defclass vertex-data ()
  ((vao :initarg :vao :accessor vao)
   (vbo :initarg :vbo :accessor vbo)
   (ebo :initarg :ebo :accessor ebo)
   (index-count :initarg :index-count :accessor index-count)
   (draw-mode :initform :triangles :accessor draw-mode)))

(defmethod delete-gl ((obj vertex-data))
  (gl:delete-buffers (list (vbo obj) (ebo obj)))
  (gl:delete-vertex-arrays (list (vao obj))))

(defun make-vertex-data (vertex-form vertices indices)
  "Create vertex data using the verticies matching vertex-form"
  (let* ((vertex-data (vertex-list-to-array vertex-form vertices))
	 (index-data (index-list-to-array indices))
	 (vao (gl:gen-vertex-array))
	 (buffers (gl:gen-buffers 2))
	 (vbo (elt buffers 0))
	 (ebo (elt buffers 1)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (%gl:buffer-data :array-buffer
		     (* (length vertices) (vertex-mem-size vertex-form))
		     vertex-data
		     :static-draw)
    (gl:bind-buffer :element-array-buffer ebo)
    (gl:buffer-data :element-array-buffer :static-draw index-data)
    (setup-vertex-attrib-array vertex-form)
    (cffi:foreign-free vertex-data)
    (gl:free-gl-array index-data)
    (make-instance 'vertex-data :vao vao :vbo vbo :ebo ebo :index-count (length indices))))

(defun draw-vertex-data (vertex-data &key (vertices 0 vertp) (instances 1))
  (if (not vertp) (setf vertices (index-count vertex-data)))
  (assert (and (> vertices 0) (> instances 0) (<= vertices (index-count vertex-data)))
	  (vertices instances)
	  "draw vertex data args were out of range. vertices: (~d) instances: (~d)"
	  vertices instances)
  (gl:bind-vertex-array (vao vertex-data))
  (if (> instances 1)
      (%gl:draw-elements-instanced (draw-mode vertex-data) vertices :unsigned-int 0 instances)
      (%gl:draw-elements (draw-mode vertex-data) vertices :unsigned-int 0)))



;;; ----- Helpers -----

(declaim (ftype (function (vertex-form list cffi:foreign-pointer integer) integer)
		buffer-vertex-data))
(defun buffer-vertex-data (vertex-form vertex pointer offset)
  (assert (equalp (length vertex) (length (vertex-slots vertex-form))) (vertex)
	  "vertex data did not match vertex form: ~a" vertex)
  (loop for slot in (vertex-slots vertex-form)
	for data in vertex do
	(assert (equalp (length data) (vector-size slot))
		(data)
		"vertex data did not match vertex form. Expected ~a values, got ~a"
		(vector-size slot) data)
	(loop for value in data do
	      (setf (cffi:mem-aref (cffi:inc-pointer pointer offset)
				   (element-type slot))
		    (case (element-type slot)
			  ((:float :double) (float value))
			  (otherwise value)))
	      (setf offset (+ offset (cffi:foreign-type-size (element-type slot))))))
  offset)

(declaim (ftype (function (vertex-form list) cffi:foreign-pointer) vertex-list-to-array))
(defun vertex-list-to-array (vertex-form vertices)
  ;;todo: make this fn actually do something
  (let* ((vertex-count (length vertices))
	 (buff (cffi:foreign-alloc :char :count (* (vertex-mem-size vertex-form) vertex-count)))
	 (offset 0))
    (loop for vertex in vertices do
	  (setf offset (buffer-vertex-data vertex-form vertex buff offset)))
    buff))

(declaim (ftype (function (list) gl:gl-array) index-list-to-array))
(defun index-list-to-array (indices)
  (let* ((count (length indices))
	 (arr (gl:alloc-gl-array :int count)))
    (loop for i from 0 for index in indices do
	  (assert (and (typep index 'integer) (>= index 0)) (index)
		  "index ~a in indices list was not a number > 0" index)
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
	     (vertex-mem-size vertex-form) (cffi:make-pointer offset)))))
