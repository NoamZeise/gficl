(defpackage :gficl/load
  (:use :cl)
  (:local-nicknames (#:obj #:org.shirakumo.fraf.wavefront)
		    (#:gltf #:org.shirakumo.fraf.gltf))
  (:export
   :model
   :image
   :shader
   :compute-shader))
