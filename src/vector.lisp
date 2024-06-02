(in-package :gficl)

(defclass vec ()
  ((data :initarg :data :type array)
   (dimension :initarg :dimension :accessor dimension)))

(defmethod print-object ((obj vec) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~a" (slot-value obj 'data))))

(declaim (ftype (function (list) (values vec &optional)) make-vec))
(defun make-vec (data)
  "create a VEC from a list of numbers"
  (let* ((length (length data)))
    (assert (> length 0) () "data passed to make-vec was of length 0.")
    (loop for elem in data do (assert (numberp elem) () "~a is not a number." elem))
    (make-instance 'vec :data (make-array length :initial-contents data) :dimension length)))

(defmacro vec-ref (vec pos)
  `(assert (and (integerp ,pos) (> pos 0) (< pos (dimension ,vec))) ()
	   "vec pos was out of range.")
  `(aref (slot-value ,vec 'data) ,pos))

(declaim (ftype (function (vec vec) number) dot))
(defun dot (v1 v2)
  "dot product of two VECs, extra dimensions are ignored"
  (loop for x across (slot-value v1 'data)
	for y across (slot-value v2 'data) summing
	(* x y)))

(declaim (ftype (function (vec &rest vec) (values vec &optional)) +vec))
(defun +vec (vec &rest vecs)
  (if (not (car vecs)) vec
    (apply #'+vec (make-vec (loop for x across (slot-value vec 'data)
				  for y across (slot-value (car vecs) 'data) collecting
				  (+ x y)))
	   (cdr vecs))))

(declaim (ftype (function (vec &optional vec) (values vec &optional)) -vec))
(defun -vec (vec1 &optional vec2)
  "negate a VEC or take one VEC away from another."
  (flet ((negate-vec (vec)
		     (make-vec (loop for x across (slot-value vec 'data) collecting (- x)))))
	(if vec2 (+vec vec1 (negate-vec vec2))
	  (negate-vec vec1))))

(declaim (ftype (function (vec) vec) normalise))
(defun normalise (vec)
  "normalise a vector"
  (let* ((mag (sqrt (dot vec vec))))
    (assert (not (= mag 0)) () "magnitude of the vector cannot be zero")
    (make-vec (loop for x across (slot-value vec 'data) collecting (/ x mag)))))

(declaim (ftype (function (vec vec) (values vec &optional)) cross))
(defun cross (v1 v2)
  (assert (= 3 (dimension v1) (dimension v2)) ()
	  "can only cross product vectors with dimension 3")
  (let ((v1 (slot-value v1 'data))
	(v2 (slot-value v2 'data)))
    (make-vec (list (- (* (aref v1 1) (aref v2 2)) (* (aref v1 2) (aref v2 1)))
		    (- (* (aref v1 2) (aref v2 0)) (* (aref v1 0) (aref v2 2)))
		    (- (* (aref v1 0) (aref v2 1)) (* (aref v1 1) (aref v2 0)))))))

(defmacro make-vec-if-list (vec)
  (if (and (listp vec) (or (equalp (car vec) 'quote) (equalp (car vec) 'list)))
      `(make-vec ,vec) vec))
