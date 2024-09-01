(in-package :gficl)

;;; --- vertex shader input form ---

(deftype vertex-elem-type ()
	 '(member :float :double :int :unsigned-int :short :unsigned-short))

(defclass vertex-slot ()
  ((index :initarg :index :accessor index :type integer)
   (data-index :initarg :data-index :accessor data-index :type integer)
   (data-offset :initarg :data-offset :accessor data-offset :type integer)
   (slot-stride :initarg :slot-stride :accessor slot-stride :type integer)
   (element-count :initarg :element-count :accessor element-count :type integer)
   (element-type :initarg :element-type :accessor element-type :type vertex-elem-type)
   (slot-active :initarg :slot-active :accessor slot-active :type boolean)))

(defmethod print-object ((obj vertex-slot) out)
   (print-unreadable-object
    (obj out :type t)
    (format out "index ~a data-index: ~a active ~a (~a ~a)"
	    (index obj) (data-index obj) (slot-active obj) (element-count obj) (element-type obj))))

(declaim (ftype (function (integer vertex-elem-type
				   &key (:slot-active boolean)
				   (:vertex-slot-index integer)
				   (:data-offset-index integer)
				   (:data-offset integer)
				   (:slot-stride integer))
			  (values vertex-slot &optional))
		make-vertex-slot))
(defun make-vertex-slot (element-count element-type &key
				     (slot-active t)
				     (vertex-slot-index -1)
				     (data-offset-index -1)
				     (data-offset -1)
				     (slot-stride -1))
  "define an element of a vertex in a shader. ie position, normal, texcoords, etc.
ELEMENT-COUNT is how many spaces this slot is (ie 3 floats, 2 ints etc...)
VERTEX-ELEM-TYPE is the type of the element (ie float int short, etc...)

SLOT-ACTIVE indicates whether the shader uses this slot or not. 

VERTEX-SLOT-INDEX is the location of the slot in the shader input.
If it is negative, it will take as it's index the position in the list
of VERTEX-SLOTs passed to MAKE-VERTEX-FORM.

DATA-OFFSET-INDEX and DATA-OFFSET are mutually exclusive

DATA-OFFSET-INDEX is the location of this vertex slots in the vertex data,
If it is negative it will be the same as VERTEX-SLOT-INDEX.
This is a convenience feature, the same result could be achieved with 
the DATA-OFFSET argument

DATA-OFFSET is the offset into the vertex data that this slot appears at.
If it is negative the offset will be the sum of the previous slots as ordered
by the DATA-OFFSET-INDEX.

SLOT-STRIDE is the distance between consecute elements of this slot.
If it is negative, the stride will be the total size of the vertex data form."
  (assert (or (< data-offset-index 0) (< data-offset 0)) ()
	  "DATA-OFFSET-INDEX (~a) and DATA-OFFSET (~a) are mutually exclusive!"
	  data-offset-index data-offset)
  (make-instance 'vertex-slot
		 :element-count element-count
		 :element-type element-type
		 :slot-active slot-active
		 :index vertex-slot-index
		 :data-index data-offset-index
		 :data-offset data-offset
		 :slot-stride slot-stride))

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
		     (setf (index slot) i))
		 (if (< (data-index slot) 0)
		     (setf (data-index slot) (index slot)))))
    (setf vertex-slots (sort vertex-slots #'(lambda (a b) (< (data-index a) (data-index b)))))
    (loop for slot in vertex-slots
	  do (progn
	       (push (if (< (data-offset slot) 0) vertex-mem-size (data-offset slot)) slot-offsets)
	       (let ((slot-size
		      (* (element-count slot) (cffi:foreign-type-size (element-type slot)))))
		 (setf vertex-mem-size (+ vertex-mem-size slot-size)))))
    (make-instance 'vertex-form :vertex-slots vertex-slots
		   :slot-offsets (nreverse slot-offsets)
		   :vertex-mem-size vertex-mem-size)))

;;; --- vertex data ---

(defclass vertex-data (gl-object)
  ;; gl-object id stores the vertex array object
  ((vbo :initarg :vbo :accessor vbo)
   (ebo :initarg :ebo :accessor ebo)
   (index-count :initarg :index-count :accessor index-count)
   (vertex-count :initarg :vertex-count :accessor vertex-count)
   (draw-mode :initform :triangles :accessor draw-mode)))

(defmethod delete-gl ((obj vertex-data))
  (gl:delete-buffers (list (vbo obj)))
  (if (ebo obj) (gl:delete-buffers (list (ebo obj))))
  (gl:delete-vertex-arrays (list (id obj)))
  (call-next-method))

(defmethod bind-gl ((obj vertex-data))
  (gl:bind-vertex-array (id obj)))

(declaim (ftype (function (vertex-form list &optional list) (values vertex-data &optional)) make-vertex-data))
(defun make-vertex-data (vertex-form vertices &optional indices)
  "Create vertex data using the verticies matching vertex-form.
I.e if a vertex is a position and a uv, three vertices looks like:
'(((0 0 0) (0 0))
  ((0 1 0) (0 1)) 
  ((1 1 1) (1 1)))
Optionally supply a list of integers as indicies for an index buffer."  
  (make-vertex-data-from-pointers
   vertex-form
   (* (length vertices) (vertex-mem-size vertex-form))
   (vertex-list-to-array vertex-form vertices)
   (length indices)
   (if indices (cffi:foreign-alloc :int :initial-contents indices) (cffi:null-pointer))))

(declaim (ftype (function (vertex-form vector &optional (or vector nil))
			  (values vertex-data &optional))
		make-vertex-data-from-vectors))
(defun make-vertex-data-from-vectors (vertex-form vertices &optional indices)
  "Create vertex data using vectors of verticies and optionally indices.
The vertex and index data is a flat vector of floats and integers, respectively.
When indicies is null or empty, an index buffer won't be created."
  (let ((index-count (length indices)))
    (make-vertex-data-from-pointers
     vertex-form
     (* (length vertices) (cffi:foreign-type-size :float))
     (cffi:foreign-alloc :float :initial-contents vertices)
     index-count
     (if (> index-count 0)
	 (cffi:foreign-alloc :int :initial-contents indices) (cffi:null-pointer)))))

(declaim (ftype (function (vertex-data &key (vertices number)))))
(defun draw-vertex-data (vertex-data &key (vertices 0 vertp) (instances 1))
  "Binds the vertex data and uses draw-xxx depending on whether the
vertex data has an index buffer or not, and whether instances is greater than 1."
  (let ((vertex-count (if (ebo vertex-data) (index-count vertex-data) (vertex-count vertex-data))))
    (if (not vertp)      
	(setf vertices vertex-count))
    (assert (and (> vertices 0) (> instances 0) (<= vertices vertex-count))
	    (vertices instances)
	    "draw vertex data args were out of range. vertices: (~d) instances: (~d)"
	    vertices instances))
  (bind-gl vertex-data)
  (if (ebo vertex-data)
      (if (> instances 1)
	  (%gl:draw-elements-instanced (draw-mode vertex-data) vertices :unsigned-int 0 instances)
	(%gl:draw-elements (draw-mode vertex-data) vertices :unsigned-int 0))
    (if (> instances 1)
	(%gl:draw-arrays-instanced (draw-mode vertex-data) 0 vertices instances)
	(%gl:draw-arrays (draw-mode vertex-data) 0 vertices))))

(declaim (ftype (function (vertex-form integer cffi:foreign-pointer integer cffi:foreign-pointer))
		make-vertex-data-from-pointers))
(defun make-vertex-data-from-pointers
    (vertex-form vertex-data-size vertex-data index-count index-data)
  "Takes vertex and index data in raw pointer form.
Vertex data is a foreign pointer, index data is a gl-array. Both are freed by this function.
The raw data must match the supplied vertex-form and the index count and vertex data size
must be accurate."
  (let* ((vao (gl:gen-vertex-array))	 	 
	 (buffers (gl:gen-buffers 2))
	 (vbo (elt buffers 0))
	 (ebo (elt buffers 1)))
    (gl:bind-vertex-array vao)
    (gl:bind-buffer :array-buffer vbo)
    (%gl:buffer-data :array-buffer
		     vertex-data-size
		     vertex-data :static-draw)
    (cond ((not (cffi:null-pointer-p index-data))
	   (gl:bind-buffer :element-array-buffer ebo)
	   (%gl:buffer-data :element-array-buffer
			    (* index-count (cffi:foreign-type-size :int))
			    index-data :static-draw)
	   (cffi:foreign-free index-data))
	  (t (gl:delete-buffers (list ebo))
	     (setf ebo nil)))
    (setup-vertex-attrib-array vertex-form)
    (cffi:foreign-free vertex-data)
    (create-gl)
    (make-instance 'vertex-data :id vao :vbo vbo :ebo ebo
		   :index-count index-count
		   :vertex-count (/ vertex-data-size (vertex-mem-size vertex-form)))))

;;; ---- Helpers ----

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
	(assert (equalp (length data) (element-count slot))
		(data)
		"vertex data did not match vertex form. Expected ~a values, got ~a"
		(element-count slot) data)
	(loop for value in data do
	      (setf (cffi:mem-aref (cffi:inc-pointer pointer offset)
				   (element-type slot))
		    (case (element-type slot)
			  ((:float :double) (float value))
			  (otherwise value)))
	      (setf offset (+ offset (cffi:foreign-type-size (element-type slot))))))
  offset)

(defun setup-vertex-attrib-array (vertex-form)
  (let ((vertex-size (vertex-mem-size vertex-form)))
    (loop for slot in (vertex-slots vertex-form)
	  for offset in (slot-offsets vertex-form)
	  when (slot-active slot) do
	  (progn
	    (gl:enable-vertex-attrib-array (index slot))
	    (gl:vertex-attrib-pointer
	     (index slot)
	     (element-count slot)
	     (element-type slot)
	     nil
	     (if (< (slot-stride slot) 0) vertex-size (slot-stride slot))
	     (cffi:make-pointer offset))))))
