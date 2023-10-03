(in-package :asdf-user)

(defsystem :gficl
  :long-name "game-framework-in-common-lisp"
  :depends-on (:cl-glfw3
	       :cl-opengl
	       :trivial-main-thread)
  :components ((:module "src"
                :components 
                ((:file "package")
		 (:file "game" :depends-on ("callbacks" "types"))
		 (:file "callbacks")
		 (:file "types")))))
