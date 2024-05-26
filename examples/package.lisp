
;#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)
;#+linux (deploy:define-library %cl-glfw3::glfw :dont-deploy t)

(defpackage gficl-examples.basic
	    (:use :cl)
	    (:export #:run))
