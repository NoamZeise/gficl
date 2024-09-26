#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defmacro defexample (name &body body)
  `(defpackage ,(make-symbol
		 (concatenate 'string
			      (symbol-name 'gficl-examples/)
			      (symbol-name name)))
	      (:use :cl)
	      (:export #:run)
	      ,@body))

(defexample minimum)
(defexample quad-spin)
(defexample cube-wave)
(defexample post-processing)
(defexample model-loading)
(defexample font)
(defexample shadows)
