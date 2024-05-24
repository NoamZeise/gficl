(in-package :asdf-user)
(defsystem :gficl-examples
  :depends-on (:gficl)
  :components ((:module "examples"
		:components
		((:file "package")
		 (:file "basic")))))
