(defpackage gficl
  (:use :cl)
  (:export

   #:with-window
   #:with-update
   #:with-render
   #:window-width
   #:window-height
   #:closed-p
   #:toggle-fullscreen
   #:set-fullscreen
   
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

   ;; vectors
   #:make-vec
   #:vec-ref
   #:dot
   #:+vec
   #:-vec
   #:normalise
   #:cross
   
   ;; matrices
   #:bind-matrix
   #:make-matrix
   #:make-matrix-from-data
   #:*mat
   #:+mat
   #:scale-matrix
   #:translation-matrix
   #:2d-rotation-matrix
   #:change-of-basis-matrix
   #:view-matrix
   #:orthographic-matrix
   #:screen-orthographic-matrix
   #:perspective-matrix
   #:screen-perspective-matrix))
