(in-package :asdf-user)

(defsystem :gficl
  :long-name "game-framework-in-common-lisp"
  :depends-on (:uiop
	       :cl-glfw3
	       :cl-opengl
	       :cffi
	       :alexandria)
  :components ((:module "src"
                :components 
                ((:file "package")
		 (:file "game" :depends-on ("types" "shader" "matrix"))
		 (:file "types")
		 (:file "matrix")
		 (:file "shader" :depends-on ("types"))
		 (:file "framebuffer" :depends-on ("types" "image"))
		 (:file "image" :depends-on ("types"))
		 (:file "vertex" :depends-on ("types"))))))
