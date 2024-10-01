(in-package :gficl/load)

(defun shader (vert-path frag-path &key (shader-folder #p""))
  "Return a GFICL:SHADER made from the text at the given files merged with the shader-folder path."
  (gficl:make-shader
   (alexandria:read-file-into-string (merge-pathnames shader-folder vert-path))
   (alexandria:read-file-into-string (merge-pathnames shader-folder frag-path))))
