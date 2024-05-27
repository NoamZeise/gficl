(in-package :gficl)

(deftype cursor-state () '(member :normal :hidden :disabled))

(defclass colour ()
  ((r :initform 0.0 :accessor r :type float)
   (g :initform 0.0 :accessor g :type float)
   (b :initform 0.0 :accessor b :type float)
   (a :initform 0.0 :accessor a :type float))
  (:documentation "colour channels intensity in range 0.0 to 1.0"))

(declaim (ftype (function (number number number number) colour) make-colour))
(defun make-colour (r g b a)
  (let ((colour (make-instance 'colour)))
    (setf (r colour) (float r))
    (setf (g colour) (float g))
    (setf (b colour) (float b))
    (setf (a colour) (float a))
    colour))

(defmacro pass-colour (fn colour)
  `(,fn (r ,colour) (g ,colour) (b ,colour) (a ,colour)))

(defparameter *colour-blank* (make-colour 0 0 0 0))

(defmethod print-object ((obj colour) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~a, ~a, ~a, ~a" (r obj) (g obj) (b obj) (a obj))))

(deftype shader-type () '(member :vertex-shader :fragment-shader))

(defclass gl-object ()
  ((id :initarg :id :initform 0 :accessor id :type integer)))

(defgeneric delete-gl (obj)
  (:documentation "delete an OpenGL object"))

(defmethod delete-gl ((obj gl-object))
  (error "This object has not implemented the delete method"))

(defmethod print-object ((obj gl-object) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "id: ~a" (id obj))))
