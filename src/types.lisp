(in-package :gficl)

(deftype cursor-state () '(member :normal :hidden :disabled))

(defclass colour ()
  ((r :initform 0 :accessor r)
   (g :initform 0 :accessor g)
   (b :initform 0 :accessor b)
   (a :initform 0 :accessor a)))

(defun make-colour (r g b a)
  (let ((colour (make-instance 'colour)))
    (setf (r colour) r)
    (setf (g colour) g)
    (setf (b colour) b)
    (setf (a colour) a)
    colour))

(defmacro pass-colour (fn colour)
  `(,fn (r ,colour) (g ,colour) (b ,colour) (a ,colour)))

(defparameter *colour-blank* (make-colour 0 0 0 0))

(deftype shader-type () '(member :vertex-shader :fragment-shader))

(defclass gl-object ()
  ((id :initarg :id :initform 0 :accessor id :type integer)))

(defgeneric delete-gl (obj)
  (:documentation "delete a gl-object"))

(defmethod delete-gl ((obj gl-object))
  (error "This object has not implemented the delete method"))
