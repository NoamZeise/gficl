(in-package :gficl)

(defclass matrix ()
  ((data :initarg :data)
   (dimension :initarg :dimension :accessor dimension))
  (:documentation "An nxn dimensional matrix"))

(defmethod print-object ((obj matrix) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~ax~a~{~%~a~}"
	   (dimension obj) (dimension obj)
	   (slot-value obj 'data))))

(declaim (ftype (function (&key (:dimension integer)
				(:element-fn (function (integer integer) number)))
			  (values matrix &optional))
		make-matrix))
(defun make-matrix (&key (dimension 4) (element-fn (lambda (i j) (if (equalp i j) 1 0))))
  "Create a matrix with a given dimension - default is identity"
  (assert ( > dimension 0) () "Dimension must be greater than zero")
  (let ((data (loop for i from 1 to dimension collecting
		    (loop for j from 1 to dimension collecting
			  (funcall element-fn i j)))))
    (make-instance 'matrix :dimension dimension :data data)))

(declaim (ftype (function (list) (values matrix &optional)) make-matrix-from-data))
(defun make-matrix-from-data (data)
  "make a MATRIX with the DATA, which is a list of rows for the matrix."
  (let ((dim (length data)))
    (loop for row in data do
	  (assert (= dim (length row)) (row)
		  "matrix data was malformed, need nxn matrix -  row data: ~a" row))
    (make-instance 'matrix :dimension dim :data data)))

;;; --- matrix operations ---

(declaim (ftype (function (matrix &rest matrix) matrix) *mat +mat))

(defun *mat (mat1 &rest mats)
  "multiply 1 or more matrices together and return the result."
  (if (not (car mats))
      mat1
    (let ((dim (dimension mat1))
	  (mat2 (car mats)))
      (declare (type matrix mat2))
      (assert (equalp dim (dimension mat2)) ()
	      "matricies had different dimensions: ~a ~a" mat1 mat2)
      (apply #'*mat
	     (make-matrix-from-data
	      ;; multiply mat1 and mat2
	      (loop for row1 in (slot-value mat1 'data) collecting
		    (loop for i from 0 to (- dim 1) collecting
			  (loop for row2 in (slot-value mat2 'data)
				for dat in row1 summing
				(* dat (nth i row2))))))
	     (cdr mats)))))

(defun +mat (mat1 &rest mats)
  "add 1 or more matrices together and return the result."
  (if (not (car mats)) mat1
    (progn (assert (equalp (dimension mat1) (dimension (car mats))) ()
		   "matricies had different dimensions: ~a ~a" mat1 (car mats))
	   (apply #'+mat
		  (make-matrix-from-data
		   (loop for row1 in (slot-value mat1 'data)
			 for row2 in (slot-value (car mats) 'data) collecting
			 (loop for v1 in row1 for v2 in row2 collecting (+ v1 v2))))
		  (cdr mats)))))

;;; --- common matrices ---

(declaim (ftype (function (vec) matrix) create-scale-matrix create-translation-matrix))

(defun create-scale-matrix (vec)
  (assert-min-dim 3 vec)
  (make-matrix-from-data
   `((,(vec-ref vec 0)  0  0 0)
     ( 0 ,(vec-ref vec 1)  0 0)
     ( 0  0 ,(vec-ref vec 2) 0)
     ( 0  0  0 1))))

(defmacro scale-matrix (vec)
  "Return a 4x4 scaling MATRIX."
  `(create-scale-matrix (make-vec-if-list ,vec)))

(defun create-translation-matrix (vec)
  (assert-min-dim 3 vec)
  (make-matrix-from-data
   `((1 0 0 ,(vec-ref vec 0))
     (0 1 0 ,(vec-ref vec 1))
     (0 0 1 ,(vec-ref vec 2))
     (0 0 0  1))))

(defmacro translation-matrix (vec)
  "Create a 4x4 translation MATRIX."
  `(create-translation-matrix (make-vec-if-list ,vec)))

(declaim (ftype (function (number) matrix) 2d-rotation-matrix))
(defun 2d-rotation-matrix (angle)
  "returns a 4x4 rotation MATRIX."
  (make-matrix-from-data
   `((,(cos angle) ,(- (sin angle)) 0 0)
     (,(sin angle) ,(cos angle) 0 0)
     (0 0 1 0)
     (0 0 0 1))))

(declaim (ftype (function (vec vec vec) (values matrix &optional)) change-of-basis-matrix))
(defun change-of-basis-matrix (v1 v2 v3)
  "Create a 4x4 change of basis MATRIX out of the 3 given axes."
  (assert-min-dim 3 v1 v2 v3)
  (make-matrix-from-data
   `((,(vec-ref v1 0) ,(vec-ref v1 1) ,(vec-ref v1 2) 0)
     (,(vec-ref v2 0) ,(vec-ref v2 1) ,(vec-ref v2 2) 0)
     (,(vec-ref v3 0) ,(vec-ref v3 1) ,(vec-ref v3 2) 0)
     (0 0 0 1))))

(declaim (ftype (function (vec vec vec) matrix) view-matrix))
(defun create-view-matrix (position-vec forward-vec world-up-vec)
  (assert-min-dim 3 position-vec forward-vec world-up-vec)
  (let* ((forward (normalise forward-vec))
	 (left (normalise (cross (normalise world-up-vec) forward)))
	 (up (cross forward left)))
    (*-mat (change-of-basis-matrix left up forward) (translation-matrix (-vec position-vec)))))

(defmacro view-matrix (position forward world-up)
  "create a 4x4 view MATRIX pointing along the FORWARD vector at the given POSITION.
FORWARD and WORLD-UP must be non-zero, and are automatically normalised."
  `(create-view-matrix (make-vec-if-list ,position)
		       (make-vec-if-list ,forward)
		       (make-vec-if-list ,world-up)))

;;; --- perspective matrices ---

(declaim (ftype (function (number number number number number number) matrix) orthographic-matrix))
(defun orthographic-matrix (top bottom left right near far)
  "create a 4x4 orthographic projection MATRIX"
  (assert (not (or (equal top bottom)
		   (equal left right)
		   (equal near far)))
	  () "ortho matrix must have top!=bottom left!=right near!=nar")
  (make-matrix-from-data
   `((,(/ 2 (- right left)) 0 0 ,(- (/ (+ right left) (- right left))))
     (0 ,(/ 2 (- top bottom)) 0 ,(- (/ (+ top bottom) (- top bottom))))
     (0 0 ,(/ -2 (- far near))  ,(- (/ (+ far near) (- far near))))
     (0 0 0 1))))

(declaim (ftype (function (number number) matrix) screen-orthographic-matrix))
(defun screen-orthographic-matrix (width height)
  "create a 4x4 orthographic projection MATRIX with depth -1 to 1 based on the screen dimension"
  (assert (and (> width 0) (> height 0)) ()
	  "ortho width and height must be positive: ~ax~a" width height)
  (ortho-matrix 0 height 0 width -1 1))

(declaim (ftype (function (number number number number number &optional number) matrix)
		perspective-matrix))
(defun perspective-matrix (top bottom left right near &optional far)
  "create a 4x4 perspective projection MATRIX, far plane at infinity if not given."
  (make-matrix-from-data
   `((,(/ (* 2 near) (- right left)) 0 ,(- (/ (+ right left) (- right left))) 0)
     (0 ,(/ (* 2 near) (- top bottom)) ,(- (/ (+ top bottom) (- top bottom))) 0)
     ,(if far
	  `(0 0 ,(/ (+ far near) (- far near)) ,(- (/ (* 2 far near) (- far near))))
	`(0 0 1 ,(- (* 2 near))))
     (0 0 1 0))))

(declaim (ftype (function (number number number number &optional number) matrix)
		screen-perspective-matrix))
(defun screen-perspective-matrix (width height fov near &optional far)
  "create a 4x4 perspective projection MATRIX, far plane at infinity if not given.
fov is in radians."
  (let* ((ratio (/ width height))
	 (angle (tan (/ fov 2.0)))
	 (right (* near ratio angle))
	 (top (* near angle)))
    (perspective-matrix top (- top) (- right) right near far)))

;;; --- set shader matrices ---

(defmacro with-foreign-matrix ((var matrix) &body body)
  `(let ((,var (make-foreign-matrix-array ,matrix)))
     ,@body
     (cffi:foreign-free ,var)))

(defgeneric bind-matrix (shader name matrix)
  (:documentation "set shader location with name to value of matrix"))

(declaim (ftype (function (shader string matrix)) bind-matrix))
(defmethod bind-matrix (shader name (matrix matrix))
  (with-foreign-matrix
   (ptr matrix)
   (let ((location (shader-loc shader name)))
     (ecase (dimension matrix)
	    (2 (%gl:uniform-matrix-2fv location 1 nil ptr))
	    (3 (%gl:uniform-matrix-3fv location 1 nil ptr))
	    (4 (%gl:uniform-matrix-4fv location 1 nil ptr))))))

;;; --- helpers ---

(defun make-foreign-matrix-array (matrix)
  (let* ((floats (list)))
    ;; OpenGL has column major matrix representation
    (loop for i from 1 to (dimension matrix) do
	  (loop for row in
		(slot-value matrix 'data)
		do (push (coerce (nth (- i 1) row) 'single-float) floats)))
    (cffi:foreign-alloc :float :initial-contents (nreverse floats))))
