(in-package :gficl)

(defparameter *active-objects* 0
  "track undelete opengl resources")

(deftype cursor-state () '(member :normal :hidden :disabled))

(deftype shader-type () '(member :vertex-shader :fragment-shader))

(deftype buffer-mask () '(member :color-buffer-bit :depth-buffer-bit :stencil-buffer-bit))

;; --- gl object parent class ---

(defclass gl-object ()
  ((id :initarg :id :initform 0 :accessor id :type integer)
   (deleted :initform nil :type boolean)))

(defun create-gl ()
  (setf *active-objects* (+ *active-objects* 1)))

(defgeneric delete-gl (obj)
  (:documentation "delete an OpenGL object"))

(defmethod delete-gl ((obj gl-object))
  (if (not (slot-value obj 'deleted))
      (setf *active-objects* (- *active-objects* 1))
    (format t "gl-object ~a deleted twice~%" obj))
  (setf (slot-value obj 'deleted) t))

(defgeneric bind-gl (obj)
  (:documentation "bind this object to the relevant opengl resource"))

(defmethod bind-gl ((obj gl-object))
  (error "This object has not implemented the bind method"))

(defmethod print-object ((obj gl-object) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "id: ~a" (id obj))))
