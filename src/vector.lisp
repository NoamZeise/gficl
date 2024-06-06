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

(defmacro make-vec-if-list (vec)
  (if (and (listp vec) (or (equalp (car vec) 'quote) (equalp (car vec) 'list)))
      `(make-vec ,vec) vec))

(declaim (ftype (function (vec vec) number) dot))
(defun dot (v1 v2)
  "dot product of two VECs, extra dimensions are ignored"
  (loop for x across (slot-value v1 'data)
	for y across (slot-value v2 'data) summing
	(* x y)))

(declaim (ftype (function (number vec) (values vec &optional)) *vec))
(defun *vec (scalar vec)
  "multiply a vector by a number"
  (make-vec (loop for x across (slot-value vec 'data) collecting (* scalar x))))

(declaim (ftype (function (vec &rest vec) (values vec &optional)) internal-+vec))
(defun internal-+vec (vec &rest vecs)
  (if (not (car vecs)) vec
    (apply #'internal-+vec (make-vec (loop for x across (slot-value vec 'data)
					   for y across (slot-value (car vecs) 'data) collecting
					   (+ x y)))
	   (cdr vecs))))

(defmacro +vec (vec &rest vecs)
  "add one or more vectors together. vecs can be given as a list of numbers."
  `(internal-+vec (make-vec-if-list ,vec) ,@(loop for v in vecs collecting `(make-vec-if-list ,v))))

(declaim (ftype (function (vec &optional vec) (values vec &optional)) internal--vec))
(defun internal--vec (vec1 &optional vec2)
  (flet ((negate-vec (vec)
		     (make-vec (loop for x across (slot-value vec 'data) collecting (- x)))))
	(if vec2 (+vec vec1 (negate-vec vec2))
	  (negate-vec vec1))))

(defmacro -vec (vec1 &optional vec2)
  "negate a VEC or take one VEC away from another."
  `(internal--vec ,@(remove nil (list `(make-vec-if-list ,vec1)
				      (if vec2 `(make-vec-if-list ,vec2))))))

(declaim (ftype (function (vec) number) internal-magnitude))
(defun internal-magnitude (vec)
  (sqrt (dot vec vec)))

(defmacro magnitude (vec)
  `(internal-magnitude (make-vec-if-list ,vec)))

(defgeneric normalise (obj)
	    (:documentation "normalise an object"))

(defmethod normalise ((vec vec))
  "normalise a vector."
  (let* ((mag (sqrt (dot vec vec))))
    (assert (not (= mag 0)) (vec) "magnitude of the vector cannot be zero")
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

(declaim (ftype (function (integer vec) (values vec &optional)) get-n-vec))
(defun get-n-vec (n vec)
  "returns a VEC with n dimensions."
  (make-vec (loop for i from 0 to (- n 1) collecting
		  (if (>= i (dimension vec)) 0 (vec-ref vec i)))))

(defmacro assert-min-dim (dim &rest vecs)
  (let ((assertions (loop for v in vecs collecting `(dimension ,v))))
    `(assert (<= ,dim ,@assertions) ()  "vecs must have at least ~a dimensions" ,dim)))

(declaim (ftype (function (vec &rest number) (values vec &optional)) add-dimension))
(defun add-dimension (vec &rest nums)
  "return a VEC with an additional dimension."
  (make-vec (nconc (loop for x across (slot-value vec 'data) collecting x) nums)))
