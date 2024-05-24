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

(defun make-matrix (dimension &key
			      (element-fn (lambda (i j)
					    (if (equalp i j) 1 0))))
  (assert ( > dimension 0) (dimension) "Dimension must be greater than zero")
  (let ((data (loop for i from 1 to dimension collecting
		    (loop for j from 1 to dimension collecting
			  (funcall element-fn i j)))))
    (make-instance 'matrix :dimension dimension :data data)))

(defun make-matrix-from-data (data)
  (let ((dim (length data)))
    (loop for row in data do
	  (assert (= dim (length row)) (data)
		  "matrix data was malformed, need n by n matrix rows: ~a" data))
    (make-instance 'matrix :dimension dim :data data)))

(defun ortho-matrix (width height near far)
  "create a 4x4 orthographic projection matrix"
  (assert (and (> width 0) (> height 0)) ()
	  "ortho width or height was not positive")
  (assert (< near far) () "near was not less than far")
  (let ((top height) (bottom 0) (left 0) (right width))
    (make-matrix-from-data
     `((,(/ 2 (- right left)) 0 0 ,(- (/ (+ right left) (- right left))))
       (0 ,(/ 2 (- top bottom)) 0 ,(- (/ (+ top bottom) (- top bottom))))
       (0 0 ,(/ 2 (- far near)) ,(- (/ (+ far near) (- far near))))
       (0 0 0 1)))))

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
  (let* ((entries (alexandria:flatten (slot-value matrix 'data)))
	 (floats (map 'list #'float entries)))
    (cffi:foreign-alloc :float :initial-contents floats)))
