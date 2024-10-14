(in-package :gficl/load)

(defun image (path &rest texture-key-args)
  "Load a png file into a texture from given path. 
TEXTURE-KEY-ARGS holds any keyword args to GFICL:MAKE-TEXTURE except :format and :data.
Returns as values a GFICL:TEXTURE, texture width, and texture height"
  (pngload:with-png-in-static-vector (png path)
    (let* ((width (pngload:width png))
	   (height (pngload:height png))
	   (len (length (pngload:data png)))
	   (bits (pngload:bit-depth png))
	   (channels (/ (* len 8) (* width height bits))))
      (cffi:with-pointer-to-vector-data (data (pngload:data png))
        (values  (apply #'gficl:make-texture
			width height
			:format (gficl:get-image-format channels)
			:data data
			texture-key-args)
		 width height)))))
