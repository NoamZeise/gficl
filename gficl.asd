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
		 (:file "game" :depends-on ("callbacks" "types" "shader"))
		 (:file "callbacks")
		 (:file "types")
		 (:file "shader" :depends-on ("gl-types"))
		 (:file "framebuffer" :depends-on ("gl-types"))
		 (:file "texture" :depends-on ("gl-types"))
		 (:file "gl-types")))))
