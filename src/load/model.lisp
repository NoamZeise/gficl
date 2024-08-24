(in-package :gficl/load)

(deftype vertex-element () '(memeber :position :normal :uv :skip))

(declaim (ftype (function ((or pathname string) &key (:vertex-form list)) list)
		model))
(defun model (model-path &key (vertex-form '(:position :normal :uv)))
  "takes a path to a model and returns a list of meshes (each mesh a VERTEX-DATA).
The vertex form is a list of VERTEX-ELEMENT (ie :position, :normal, :uv, or :skip)"
  (loop for mesh in (obj:extract-meshes (obj:parse (probe-file model-path))) collecting
	(gficl:make-vertex-data-from-vectors
	 (get-vertex-form mesh vertex-form)
	 (obj:vertex-data mesh) (obj:index-data mesh))))

;;; ---- helpers ----

(defun get-vertex-form (mesh form)
  (gficl:make-vertex-form
   (loop for a in (obj:attributes mesh) collecting
	 (ecase a (:position (gficl:make-vertex-slot 3 :float))
		(:normal (gficl:make-vertex-slot 3 :float))
		(:uv (gficl:make-vertex-slot 2 :float))))))
