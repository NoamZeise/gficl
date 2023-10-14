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
		 (:file "game" :depends-on ("types" "shader"))
		 (:file "types")
		 (:file "shader" :depends-on ("types"))
		 (:file "framebuffer" :depends-on ("types" "texture"))
		 (:file "texture" :depends-on ("types"))))))
