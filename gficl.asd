(in-package :asdf-user)

(defsystem :gficl
  :long-name "game-framework-in-common-lisp"
  :depends-on (:uiop
	       :cl-glfw3
	       :cl-opengl
	       :cffi)
  :components ((:module "src"
                :components 
                ((:file "package")
		 (:file "gficl" :depends-on ("matrix" "vertex" "framebuffer"))
		 (:file "types")
		 (:file "matrix" :depends-on ("shader"))
		 (:file "shader" :depends-on ("types"))
		 (:file "framebuffer" :depends-on ("types" "image"))
		 (:file "image" :depends-on ("types"))
		 (:file "vertex" :depends-on ("types"))))))
