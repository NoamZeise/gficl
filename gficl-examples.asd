(require 'asdf)
(in-package :asdf-user)
(defsystem :gficl-examples
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "basic"
  :entry-point "gficl-examples.basic:run"
  :depends-on (:gficl)
  :components ((:module "examples"
		:components
		((:file "package")
		 (:file "quad-spin")
		 (:file "cube-wave")
		 (:file "post-processing")))))
