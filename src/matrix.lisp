(in-package :gficl)

(defclass matrix ()
  ((data :initarg :data)
   (dimension :initarg :dimension :accessor dimension))
  (:documentation "N dimensional matrix"))

(defmethod print-object ((obj matrix) out)
  (print-unreadable-object
   (obj out :type t)
   (format out "~ax~a~{~%~a~}"
	   (dimension obj) (dimension obj)
	   (slot-value obj 'data))))

(defun make-matrix (dimension &key (element-fn (lambda (i j) (if (equalp i j) 1 0))))
  "Create a matrix with a given dimension - default is identity"
  (assert ( > dimension 0) (dimension) "Dimension must be greater than zero")
  (let ((data (loop for i from 1 to dimension collecting
		    (loop for j from 1 to dimension collecting
			  (funcall element-fn i j)))))
    (make-instance 'matrix :dimension dimension :data data)))

(declaim (ftype (function (list) (values matrix &optional)) make-matrix-from-data))
(defun make-matrix-from-data (data)
  "data is a list of rows for the matrix"
  (let ((dim (length data)))
    (loop for row in data do
	  (assert (= dim (length row)) (row)
		  "matrix data was malformed, need nxn matrix -  row data: ~a" row))
    (make-instance 'matrix :dimension dim :data data)))


;;; --- matrix operations ---

(declaim (ftype (function (matrix &rest matrix) matrix) *-mat))
(defun *-mat (mat1 &rest mats)
  (if (not (car mats))
      mat1
    (let ((dim (dimension mat1))
	  (mat2 (car mats)))
      (declare (type matrix mat2))
      (assert (equalp dim (dimension mat2)) ()
	      "matricies had different dimensions: ~a ~a" mat1 mat2)
      (apply #'*-mat
	     (make-matrix-from-data
	      ;; multiply mat1 and mat2
	      (loop for row1 in (slot-value mat1 'data) collecting
		    (loop for i from 0 to (- dim 1) collecting
			  (loop for row2 in (slot-value mat2 'data)
				for dat in row1 summing
				(* dat (nth i row2))))))
	     (cdr mats)))))

;;; --- common matrices ---

(declaim (ftype (function (number number number) matrix) scale-matrix translation-matrix))

(defun scale-matrix (x y z)
  "return a 4x4 scaling matrix"
  (make-matrix-from-data
   `((,x  0  0 0)
     ( 0 ,y  0 0)
     ( 0  0 ,z 0)
     ( 0  0  0 1))))

(defun translation-matrix (x y z)
  "returns a 4x4 translation matrix"
  (make-matrix-from-data
   `((1 0 0 ,x)
     (0 1 0 ,y)
     (0 0 1 ,z)
     (0 0 0  1))))

(declaim (ftype (function (number) matrix) 2d-rotation-matrix))
(defun 2d-rotation-matrix (angle)
  "returns a 4x4 rotation matrix"
  (make-matrix-from-data
   `((,(cos angle) ,(- (sin angle)) 0 0)
     (,(sin angle) ,(cos angle) 0 0)
     (0 0 1 0)
     (0 0 0 1))))

(declaim (ftype (function (number number number number number number) matrix)
		ortho-matrix))
(defun ortho-matrix (top bottom left right near far)
  "create a 4x4 orthographic projection matrix"
  (assert (not (or (equal top bottom)
		   (equal left right)
		   (equal near far)))
	  (top bottom left right near far)
	  "ortho matrix must have top!=bottom left!=right near!=nar")
  (make-matrix-from-data
   `((,(/ 2 (- right left)) 0 0 ,(- (/ (+ right left) (- right left))))
     (0 ,(/ 2 (- top bottom)) 0 ,(- (/ (+ top bottom) (- top bottom))))
     (0 0 ,(/ -2 (- far near))  ,(- (/ (+ far near) (- far near))))
     (0 0 0 1))))

(declaim (ftype (function (number number) matrix) screen-ortho-matrix))
(defun screen-ortho-matrix (width height)
  "return an orthogonal projection matrix with depth 0 to 1"
  (assert (and (> width 0) (> height 0)) (width height)
	  "ortho width and height must be positive: ~ax~a" width height)
  (ortho-matrix 0 height 0 width 0 -1))

;;; --- set shader matricies ---

(defmacro with-foreign-matrix ((var matrix) &body body)
  `(let ((,var (make-foreign-matrix-array ,matrix)))
     ,@body
     (cffi:foreign-free ,var)))

(defgeneric set-shader-matrix (shader name matrix)
	    (:documentation "set shader location with name to value of matrix"))

(declaim (ftype (function (shader string matrix))))
(defmethod set-shader-matrix (shader name (matrix matrix))
  (with-foreign-matrix
   (ptr matrix)
   (let ((location (shader-loc shader name)))
     (ecase (dimension matrix)
	    (2 (%gl:uniform-matrix-2fv location 1 nil ptr))
	    (3 (%gl:uniform-matrix-3fv location 1 nil ptr))
	    (4 (%gl:uniform-matrix-4fv location 1 nil ptr))))))

;;; ---- Helpers ----

(defun make-foreign-matrix-array (matrix)
  (let* ((floats (list)))
    ;; OpenGL has column major matrix representation
    (loop for i from 1 to (dimension matrix) do
	  (loop for row in
		(slot-value matrix 'data)
		do (push (coerce (nth (- i 1) row) 'single-float) floats)))
    (cffi:foreign-alloc :float :initial-contents (nreverse floats))))
