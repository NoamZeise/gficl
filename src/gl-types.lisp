(in-package :gficl)

(deftype shader-type () '(member :vertex-shader :fragment-shader))

(defclass gl-object ()
  ((id :initarg :id :initform 0 :accessor id :type integer)))

(defgeneric delete-gl (obj)
  (:documentation "delete a gl-object"))

(defmethod delete-gl ((obj gl-object))
  (error "This object has not implemented the delete method"))
