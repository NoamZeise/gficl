(in-package :gficl)

(deftype cursor-state () '(member :normal :hidden :disabled))

(deftype shader-type () '(member :vertex-shader :fragment-shader :compute-shader))

(deftype buffer-mask () '(member :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))

;; --- gl object parent class ---

(defclass gl-object ()
  ((id :initarg :id :initform 0 :accessor id :type integer)
   (deleted :initform nil :type boolean))
  (:documentation "A handle for some opengl resource that must be manually freed."))

(defparameter *active-objects* 0
  "track undelete opengl resources")

(defun create-gl ()
  "GL-OBJECT constructors must call this method to increment the number of *ACTIVE-OBJECTS*.
*ACTIVE-OBJECTS* is decremented when DELETE-GL is called for the first time on a GL-OBJECT.
If the number of active objects is greater than 0 when the program exits (checked in WITH-WINDOW) then a warning is printed."
  (setf *active-objects* (+ *active-objects* 1)))

(defgeneric delete-gl (obj)
  (:documentation "Delete a GL-OBJECT returned by any function that begins with `MAKE-`.
A warning is printed on program exit if not all GL-OBJECT instances have been passed to this method."))

(defmethod delete-gl ((obj gl-object))	   
  (if (not (slot-value obj 'deleted))
      (setf *active-objects* (- *active-objects* 1))
    (format t "gl-object ~a deleted twice~%" obj))
  (setf (slot-value obj 'deleted) t))

(defgeneric bind-gl (obj)
  (:documentation "Bind this object in opengl. 
This will be different for each resource that inherits from GL-OBJECT, but there is usually an 'obvious' way to bind certain objects in opengl."))

(defmethod bind-gl ((obj gl-object))
  (error "This object has not implemented the BIND-GL method. 
It may not be a relevant operation for that particular opengl resource."))

(defmethod print-object ((obj gl-object) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "id: ~a" (id obj))))
