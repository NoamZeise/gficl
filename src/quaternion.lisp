(in-package :gficl)

(defclass quaternion ()
  ((re :initarg :re :accessor re-quat :type number)
   (im :initarg :im :accessor im-quat :type vector))
  (:documentation "A quaternion with a real number and an imaginary VEC"))

(declaim (ftype (function (number vec) (values quaternion &optional))
		internal-make-quat
		internal-make-unit-quat))

(defun internal-make-quat (real imaginary)
  "create a QUATERNION object."
  (assert (= 3 (dimension imaginary)) () "imaginary part of quat must have exactly 3 dimensions.")
  (make-instance 'quaternion :re real :im imaginary))

(defmacro make-quat (real imaginary)
  "create a QUATERNION object. imaginary can be a vector or a list of three numbers."
  `(internal-make-quat ,real (make-vec-if-list ,imaginary)))

(defun internal-make-unit-quat (angle vector)
  (let ((v (normalise vector)))
    (make-quat (cos angle) (*vec (sin angle) v))))

(defmacro make-unit-quat (angle vector)
  "create a unit QUATERNION using an angle in radians and a 3 dimensional vector 
or a list of 3 numbers."
  `(internal-make-unit-quat ,angle (make-vec-if-list ,vector)))

;;; --- quaternion operations ---

(declaim (ftype (function (quaternion &rest quaternion) (values quaternion &optional)) *quat))
(defun *quat (q &rest quats)
  "multiply quaternions together"
  (if (not (car quats)) q
    (let ((r (car quats)))
      (assert (typep r 'quaternion) (r) "can only multiply quaternions.")
      (apply #'*quat
	     (make-quat (-  (* (re-quat q) (re-quat r))
			    (dot (im-quat q) (im-quat r)))
			(+vec (cross (im-quat q) (im-quat r))
			      (*vec  (re-quat r) (im-quat q))
			      (*vec  (re-quat q) (im-quat r))))
	     (cdr quats)))))

(declaim (ftype (function (quaternion) (values quaternion &optional)) quat-conjugate))
(defun quat-conjugate (quat)
  "return the conjugate of a QUATERNION."
  (make-quat (re-quat quat) (-vec (im-quat quat))))

(declaim (ftype (function (quaternion &optional boolean) (values vec &optional)) quat-to-vec))
(defun quat-to-vec (quat &optional 3-dimensional-vec)
  (if 3-dimensional-vec (add-dimension (im-quat quat) (re-quat quat))
    (im-quat quat)))

(defun quat-conjugate-vec (quat vec)
  "conjugate a VEC by a QUATERNION. must be a unit quaternion.
returns 3 dimensional VEC if vec has dimension less than 4. 
otherwise returns a 4 dimensional VEC."
  (let* ((d (dimension vec))
	 (v (get-n-vec 3 vec))
	 (q (if (> d 3)
		(make-quat (vec-ref vec 3) v)
	      (make-quat 0 v))))
    (quat-to-vec (*quat quat q (quat-conjugate quat)) (> d 3))))

(declaim (ftype (function (vec number vec) (values vec &optional)) rotate-vec))
(defun rotate-vec (point-vec angle axis)
  "Rotate a point VEC around an axis VEC by an angle in radians."
  (quat-conjugate-vec (make-unit-quat angle axis) point-vec))
