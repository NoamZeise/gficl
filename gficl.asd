(require 'asdf)
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
		 (:file "gficl" :depends-on ("input"))
		 (:file "input" :depends-on ("state"))
		 (:file "state")
		 (:file "types")
		 (:file "vector")
		 (:file "quaternion" :depends-on ("vector"))
		 (:file "matrix" :depends-on ("quaternion" "shader"))
		 (:file "shader" :depends-on ("types"))
		 (:file "framebuffer" :depends-on ("types" "image"))
		 (:file "image" :depends-on ("types"))
		 (:file "vertex" :depends-on ("types"))
		 (:file "buffer" :depends-on ("types"))))))

(defsystem :gficl/load
  :depends-on (:gficl
	       ;; model
	       :cl-wavefront
	       :cl-gltf
	       ;; images
	       :pngload
	       :cl-jpeg)
  :components ((:module "src/load"
		:components
		((:file "package")
		 (:file "model")
		 (:file "shader")
		 (:file "image")))))
