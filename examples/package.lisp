#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage gficl-examples.quad-spin
	    (:use :cl)
	    (:export #:run))

(defpackage gficl-examples.cube-wave
	    (:use :cl)
	    (:export #:run))
