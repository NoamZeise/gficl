(defpackage gficl
  (:use :cl)
  (:export

   #:with-window
   #:with-update
   #:with-render
   #:window-width
   #:window-height
   #:closed-p
   
   ;; gl objects
   #:id
   #:bind-gl
   #:delete-gl

   ;; vertex data description
   #:make-vertex-slot
   #:make-vertex-form
   ;; vertex data
   #:make-vertex-data
   #:draw-vertex-data

   ;; image
   #:make-texture
   #:make-texture-with-fn
   #:get-image-format

   ;; framebuffer
   #:make-attachment-description
   #:make-framebuffer
   #:blit-framebuffers
   #:framebuffer-texture-id

   ;; shader
   #:make-shader
   #:make-shader-from-path
   #:shader-loc

   ;; matrices
   #:bind-matrix
   #:make-matrix
   #:make-matrix-from-data
   #:*-mat
   #:scale-matrix
   #:translation-matrix
   #:2d-rotation-matrix
   #:ortho-matrix
   #:screen-ortho-matrix))
