(in-package :gficl)

;;; --- vertex shader input form ---

(deftype vertex-elem-type ()
	 '(member :float :double :int :unsigned-int :short :unsigned-short))

(defclass vertex-slot ()
  ((index :initarg :index :accessor index :type integer)
   (vector-size :initarg :vector-size :accessor vector-size :type integer)
   (element-type :initarg :element-type :accessor element-type :type vertex-elem-type)
   (slot-active :initarg :slot-active :accessor slot-active :type boolean)))

(defmethod print-object ((obj vertex-slot) out)
   (print-unreadable-object
    (obj out :type t)
    (format out "index ~a active ~a (~a ~a)"
	    (index obj) (slot-active obj) (vector-size obj) (element-type obj))))

(declaim (ftype (function (integer vertex-elem-type
				   &key (:vertex-slot-index integer)
				   (:slot-active boolean))
			  (values vertex-slot &optional))
		make-vertex-slot))
(defun make-vertex-slot (vector-size element-type &key
				     (vertex-slot-index -1)
				     (slot-active t))
  "define an element of a vertex in a shader. ie position, normal, texcoords, etc.
VECTOR-SIZE is how many spaces this slot is (ie 3 floats, 2 ints etc...)
VERTEX-ELEM-TYPE is the type of the element (ie float int short, etc...)
If VERTEX-SLOT-INDEX is negative, it will take as it's index the position in the list
of VERTEX-SLOTs passed to MAKE-VERTEX-FORM.
SLOT-ACTIVE indicates whether the shader uses this slot or not. "
  (make-instance 'vertex-slot
		 :vector-size vector-size
		 :element-type element-type
		 :index vertex-slot-index
		 :slot-active slot-active))

(defclass vertex-form ()
  ((vertex-slots :initarg :vertex-slots :accessor vertex-slots)
   (slot-offsets :initarg :slot-offsets :accessor slot-offsets)
   (vertex-mem-size :initarg :vertex-mem-size :accessor vertex-mem-size)))

(defmethod print-object ((obj vertex-form) out)
   (print-unreadable-object
    (obj out :type t)
    (format out "size: ~a offsets: ~a~%slots: ~a"
	    (vertex-mem-size obj) (slot-offsets obj) (vertex-slots obj))))

(defun make-vertex-form (vertex-slots)
  "Define a vertex shader input based on the supplied vertex slots.
For any slots with undefined indices the order must match the vertex locations in the shader"
  (let* ((slot-offsets (list))
	 (vertex-mem-size 0))
    (loop for i from 0 for slot in vertex-slots do
	  (progn (assert (typep slot 'vertex-slot))
		 (if (< (index slot) 0)
		     (setf (index slot) i))))
    (setf vertex-slots (sort vertex-slots #'(lambda (a b) (< (index a) (index b)))))
    (loop for slot in vertex-slots
	  do (progn
	       (push vertex-mem-size slot-offsets)
	       (let ((slot-size
		      (* (vector-size slot) (cffi:foreign-type-size (element-type slot)))))
		 (setf vertex-mem-size (+ vertex-mem-size slot-size)))))
    (make-instance 'vertex-form :vertex-slots vertex-slots
		   :slot-offsets (nreverse slot-offsets)
		   :vertex-mem-size vertex-mem-size)))

;;; --- vertex data ---

(defclass vertex-data (gl-object)
  ;; gl-object id store vertex array object
  ((vbo :initarg :vbo :accessor vbo)
   (ebo :initarg :ebo :accessor ebo)
   (index-count :initarg :index-count :accessor index-count)
   (draw-mode :initform :triangles :accessor draw-mode)))

(defmethod delete-gl ((obj vertex-data))
  (gl:delete-buffers (list (vbo obj) (ebo obj)))
  (gl:delete-vertex-arrays (list (id obj)))
  (call-next-method))

(defmethod bind-gl ((obj vertex-data))
  (gl:bind-vertex-array (id obj)))

(declaim (ftype (function (vertex-form list list) (values vertex-data &optional)) make-vertex-data))
(defun make-vertex-data (vertex-form vertices indices)
  "Create vertex data using the verticies matching vertex-form.
I.e if a vertex is a position and a uv, three vertices looks like:
'(((0 0 0) (0 0))
  ((0 1 0) (0 1)) 
  ((1 1 1) (1 1)))"  
  (make-vertex-data-from-pointers
   vertex-form
   (* (length vertices) (vertex-mem-size vertex-form))
   (vertex-list-to-array vertex-form vertices)
   (length indices)
   (cffi:foreign-alloc :int :initial-contents indices)))

(declaim (ftype (function (vertex-form vector vector)
			  (values vertex-data &optional))
		make-vertex-data-from-vectors))
(defun make-vertex-data-from-vectors (vertex-form vertices indices)
  "Create vertex data using vectors of verticies and indices.
So the vertex and index data is just a flat vector of floats and integers, respectively."
  (make-vertex-data-from-pointers
   vertex-form
   (* (length vertices) (cffi:foreign-type-size :float))
   (cffi:foreign-alloc :float :initial-contents vertices)
   (length indices) (cffi:foreign-alloc :int :initial-contents indices)))

(declaim (ftype (function (vertex-data &key (vertices number)))))
(defun draw-vertex-data (vertex-data &key (vertices 0 vertp) (instances 1))
  (if (not vertp) (setf vertices (index-count vertex-data)))
  (assert (and (> vertices 0) (> instances 0) (<= vertices (index-count vertex-data)))
	  (vertices instances)
	  "draw vertex data args were out of range. vertices: (~d) instances: (~d)"
	  vertices instances)
  (bind-gl vertex-data)
  (if (> instances 1)
      (%gl:draw-elements-instanced (draw-mode vertex-data) vertices :unsigned-int 0 instances)
    (%gl:draw-elements (draw-mode vertex-data) vertices :unsigned-int 0)))

;;; ----- Helpers -----

(declaim (ftype (function (vertex-form integer cffi:foreign-pointer integer cffi:foreign-pointer))
		make-vertex-data-from-pointers))
(defun make-vertex-data-from-pointers
    (vertex-form vertex-data-size vertex-data index-count index-data)
  "Takes vertex and index data in raw pointer form.
vertex data is a foreign pointer, index data is a gl-array. Both are freed by this function."
  (let* ((vao (gl:gen-vertex-array))	 	 
	 (buffers (gl:gen-buffers 2))
	 (vbo (elt buffers 0))
	 (ebo (elt buffers 1)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (%gl:buffer-data :array-buffer
		     vertex-data-size
		     vertex-data :static-draw)
    (gl:bind-buffer :element-array-buffer ebo)
    (%gl:buffer-data :element-array-buffer
		     (* index-count (cffi:foreign-type-size :int))
		     index-data :static-draw)
    (setup-vertex-attrib-array vertex-form)
    (cffi:foreign-free vertex-data)
    (cffi:foreign-free index-data)
    (create-gl)
    (make-instance 'vertex-data :id vao :vbo vbo :ebo ebo :index-count index-count)))

(declaim (ftype (function (vertex-form list) cffi:foreign-pointer) vertex-list-to-array))
(defun vertex-list-to-array (vertex-form vertices)
  "Create foreign memory to hold the vertex data in.
Move vertex data vertex by vertex and check that the form matches the vertex form."
  (let* ((vertex-count (length vertices))
	 (buff (cffi:foreign-alloc :char :count (* (vertex-mem-size vertex-form) vertex-count)))
	 (offset 0))
    (loop for vertex in vertices do
	  (setf offset (buffer-vertex-data vertex-form vertex buff offset)))
    buff))

(defun buffer-vertex-data (vertex-form vertex pointer offset)
  "Destructures a vertex list and check it matches the vertex form. Copy to foreign memory."
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

(defun setup-vertex-attrib-array (vertex-form)
  (loop for slot in (vertex-slots vertex-form)
	for offset in (slot-offsets vertex-form)
	when (slot-active slot) do
	(progn
	  (gl:enable-vertex-attrib-array (index slot))
	  (gl:vertex-attrib-pointer
	   (index slot)
	   (vector-size slot)
	   (element-type slot)
	   nil
	   (vertex-mem-size vertex-form)
	   (cffi:make-pointer offset)))))
