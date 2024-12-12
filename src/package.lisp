(defpackage gficl
  (:use :cl)
  (:export

   #:with-window
   #:with-update
   #:with-render
   #:window-width
   #:window-height
   #:closedp
   #:toggle-fullscreen
   #:set-fullscreen

   ;; input
   #:key-down
   #:key-pressed
   #:map-keys-down
   #:map-keys-pressed

   ;;hardware querying
   #:msaa-samples
   
   ;; gl objects
   #:gl-object
   #:id
   #:bind-gl
   #:delete-gl

   ;; vertex data description
   #:vertex-form
   #:make-vertex-slot
   #:make-vertex-form
   ;; vertex data
   #:vertex-data
   #:make-vertex-data
   #:make-vertex-data-from-vectors
   #:make-vertex-data-from-pointers
   #:draw-vertex-data

   ;; image
   #:texture
   #:make-texture
   #:make-texture-with-fn
   #:get-image-format

   ;; framebuffer attachment description
   #:attachment-description
   #:make-attachment-description
   #:attach-desc-type
   #:attach-desc-clear-bits
   ;; framebuffer
   #:framebuffer
   #:make-framebuffer
   #:blit-framebuffers
   #:framebuffer-texture-id
   #:framebuffer-add-external-attachment

   ;; shader
   #:shader
   #:make-shader
   #:make-compute-shader
   #:make-shader-from-path
   #:shader-loc

   ;; vectors
   #:vec
   #:bind-vec
   #:make-vec
   #:vec-ref
   #:=vec
   #:dot
   #:+vec
   #:-vec
   #:*vec
   #:magnitude
   #:normalise
   #:cross
   #:get-n-vec

   ;; quaternion
   #:quat
   #:make-quat
   #:make-unit-quat
   #:*quat
   #:quat-conjugate
   #:quat-to-vec
   #:quat-conjugate-vec
   #:rotate-vec
   
   ;; matrices
   #:matrix
   #:bind-matrix
   #:make-matrix
   #:make-matrix-from-data
   #:*mat
   #:+mat
   #:scalar*mat
   #:transpose-matrix
   #:cofactor-matrix
   #:determinant
   #:inverse-matrix
   ;; constructors for useful matrices
   #:scale-matrix
   #:translation-matrix
   #:2d-rotation-matrix
   #:change-of-basis-matrix
   #:view-matrix
   #:orthographic-matrix
   #:screen-orthographic-matrix
   #:perspective-matrix
   #:screen-perspective-matrix
   #:target-resolution-matrix))
