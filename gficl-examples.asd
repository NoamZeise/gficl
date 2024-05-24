(in-package :asdf-user)
(defsystem :gficl-examples
  :build-operation "program-op"
  :build-pathname "basic"
  :entry-point "gficl-examples.basic:run"
  :depends-on (:gficl)
  :components ((:module "examples"
		:components
		((:file "package")
		 (:file "basic")))))
