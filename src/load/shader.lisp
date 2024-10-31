(in-package :gficl/load)

(defun shader (vert-path frag-path &key (shader-folder #p""))
  "Return a GFICL:SHADER made from the text at the given files merged with the shader-folder path."
  (let ((folder (merge-pathnames shader-folder)))
    (gficl:make-shader
     (alexandria:read-file-into-string (merge-pathnames vert-path folder))
     (alexandria:read-file-into-string (merge-pathnames frag-path folder)))))
