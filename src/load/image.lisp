(in-package :gficl/load)

(defun image (path &rest texture-key-args)
  "Load an image file into a texture from given path. 
TEXTURE-KEY-ARGS holds any keyword args to GFICL:MAKE-TEXTURE except :format and :data.
Returns as values a GFICL:TEXTURE, texture width, and texture height"
  (str-case (pathname-type path)
	       ("png" (image-png path texture-key-args))
	       (("jpeg" "jpg") (image-jpeg path texture-key-args))
	       (t (format t "unrecognised image extension. trying png~%")
		  (image-png path texture-key-args))))

;;; ---- Image Format Loaders ----

(defun image-png (path texture-key-args)
  (pngload:with-png-in-static-vector (png path)
    (let* ((width (pngload:width png))
	   (height (pngload:height png))
	   (len (length (pngload:data png)))
	   (bits (pngload:bit-depth png))
	   (channels (/ (* len 8) (* width height bits))))
      (cffi:with-pointer-to-vector-data (data (pngload:data png))
        (values  (apply #'gficl:make-texture
			width height
			:format (gficl:get-image-format channels)
			:data data
			texture-key-args)
		 width height)))))

(defun image-jpeg (path texture-key-args)
  (multiple-value-bind (data h w) (cl-jpeg:decode-image path)
    (let* ((len (length data))
	   (channels (/ len w h)))
      (assert (= channels 3) ()
	      "only jpeg images with three channels supported, given ~a"
	      channels)
      (loop-vec (b g r) data
		(let ((x r) (y g) (z b)) (setf b x) (setf g y) (setf r z)))
      (cffi:with-pointer-to-vector-data (ptr data)
					(values (apply #'gficl:make-texture w h :format
						       (gficl:get-image-format channels)
						       :data ptr texture-key-args)
						w h)))))

;;; ---- Helper Functions ----

(defmacro str-case (extension &body cases)
  `(cond
    ,@(loop for c in cases
	    when (eq t (car c)) collecting
	    `(t ,@(cdr c))
	    else
	    when (and (listp (car c)) (car c)) collecting
	    `((or ,@(loop for val in (car c) collecting `(equalp ,val ,extension))) ,@(cdr c)) else collecting
	    `((equalp ,(car c) ,extension) ,@(cdr c)))))

(defmacro loop-vec (vars vector &body body)
  (let ((i (gensym)) (l (gensym)) (n (length vars)))
    `(let ((,i 0) (,l (length ,vector)))
       (assert (= 0 (mod ,l ,n)) ()
	       "number of loop-vec vars (~a) needs to evenly divide the length of the vector (~a)"
	       ,n ,l)
       (loop until (>= ,i ,l)
	     do
	     (symbol-macrolet
	      ,(loop for count from 0 for v in vars collecting
		     (list v `(aref ,vector (+ ,i ,count))))
	      ,@body
	      (setf ,i (+ ,i ,n))))
       ,vector)))
