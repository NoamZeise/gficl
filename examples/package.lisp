#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage gficl-examples.basic
	    (:use :cl)
	    (:export #:run))
