#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defpackage gficl-examples.quad-spin
	    (:use :cl)
	    (:export #:run))

(defpackage gficl-examples.cube-wave
	    (:use :cl)
	    (:export #:run))

(defpackage gficl-examples.post-processing
	    (:use :cl)
	    (:export #:run))

(defpackage gficl-examples.model-loading
	    (:use :cl)
	    (:local-nicknames (#:obj #:org.shirakumo.fraf.wavefront))
	    (:export #:run))

(defpackage gficl-examples.font
	    (:use :cl)
	    (:export #:run))
