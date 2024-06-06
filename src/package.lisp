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

   ;; input
   #:key-down
   #:key-pressed
   #:map-keys-down
   #:map-keys-pressed
   
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
   #:*vec
   #:magnitude
   #:normalise
   #:cross
   #:get-n-vec

   ;; quaternion
   #:make-quat
   #:make-unit-quat
   #:*quat
   #:quat-conjugate
   #:quat-to-vec
   #:quat-conjugate-vec
   
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
