(defpackage gficl
  (:use :cl)
  (:export

   #:with-game
   #:with-update
   #:with-render
   #:set-game-should-close
   #:game-closed-p
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

   ;; shader
   #:make-shader-from-path
   #:make-shader

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
