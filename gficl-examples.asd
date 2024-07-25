(require 'asdf)
(in-package :asdf-user)
(defsystem :gficl-examples
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "cube-wave"
  :entry-point "gficl-examples.cube-wave:run"
  :depends-on (:gficl
	       ;; obj model loading
	       :cl-wavefront)
  :components ((:module "examples"
		:components
		((:file "package")
		 (:file "quad-spin")
		 (:file "cube-wave")
		 (:file "post-processing")
		 (:file "model-loading")))))
