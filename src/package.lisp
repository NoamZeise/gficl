(defpackage gficl
  (:use :cl)
  (:export

   #:with-window
   #:with-update
   #:with-render
   #:closed-p
   #:delete-gl

   ;; colour
   #:colour
   #:make-colour

   ;; vertex data description
   #:make-vertex-slot
   #:make-vertex-form
   ;; vertex data
   #:make-vertex-data
   #:draw-vertex-data

   ;; image

   ;; framebuffer

   ;; shader
   #:make-shader
   #:make-shader-from-path
   #:shader-loc

   ;; matrices
   #:set-shader-matrix
   #:make-matrix
   #:make-matrix-from-data
   #:*-mat
   #:scale-matrix
   #:translation-matrix
   #:2d-rotation-matrix
   #:ortho-matrix
   #:screen-ortho-matrix))
