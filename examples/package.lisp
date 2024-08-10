#+linux (deploy:define-library cl-opengl-bindings::opengl :dont-deploy t)

(defmacro defexample (name &body body)
  `(defpackage ,name
	      (:use :cl)
	      (:export #:run)
	      ,@body))

(defexample gficl-examples.quad-spin)

(defexample gficl-examples.cube-wave)

(defexample gficl-examples.post-processing)

(defexample gficl-examples.model-loading
	    (:local-nicknames (#:obj #:org.shirakumo.fraf.wavefront)))

(defexample gficl-examples.font)

(defexample gficl-examples.shadows
	    (:local-nicknames (#:obj #:org.shirakumo.fraf.wavefront)))
