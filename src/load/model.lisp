(in-package :gficl/load)

(deftype vertex-element () '(memeber :position :normal :uv :skip))

(declaim (ftype (function ((or pathname string) &key (:vertex-form list)) list)
		model))
(defun model (model-path &key (vertex-form '(:position :normal :uv)))
  "takes a path to a model and returns a list of meshes (each mesh a VERTEX-DATA instance).
:vertex-form is a list of VERTEX-ELEMENT (ie :position, :normal, :uv, or :skip)"
  (loop for mesh in (obj:extract-meshes (obj:parse (probe-file model-path))) collecting
	(gficl:make-vertex-data-from-vectors
	 (get-vertex-form mesh vertex-form)
	 (obj:vertex-data mesh) (obj:index-data mesh))))

(defun test ()
  (gltf:with-gltf (torus (probe-file #p"examples/assets/torus.glb"))
		  (describe (gltf:buffers torus))
		  (describe (gltf:buffer-views torus))
		  (loop for mesh across (gltf:meshes torus) do
			(loop for mp across (gltf:primitives mesh) do
			      (loop for key being the hash-keys of (gltf:attributes mp)
				    using (hash-value attribute) do
				    (let ((view (gltf:buffer-view attribute)))
				      (format t "~a:~%" key)
				      (describe view)))))))

;;; ---- helpers ----

(defun get-vertex-form (mesh form)
  (gficl:make-vertex-form
   (loop for i from 0 for a in (obj:attributes mesh) collecting
	 (destructuring-bind (type size)
			     (ecase a
				    (:position '(:float 3))
				    (:normal '(:float 3))
				    (:uv '(:float 2)))
			     (let ((pos (position a form)))
			       (gficl:make-vertex-slot
				size type
				:vertex-slot-index (if pos pos -1)
				:data-offset-index i
				:slot-active (if pos t nil)))))))
