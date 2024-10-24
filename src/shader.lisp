(in-package :gficl)

(defclass shader (gl-object) ())

(declaim (ftype (function (pathname pathname) shader) make-shader-from-path))
(defun make-shader-from-path (vertex-path fragment-path)
  "First load the shader code from the files specified, then
create a shader using the code loaded.
Must be manually freed by calling DELETE-GL"
  (let ((vertex-code (uiop:read-file-string vertex-path))
	(fragment-code (uiop:read-file-string fragment-path)))
    (make-shader vertex-code fragment-code)))

(declaim (ftype (function (string string) shader) make-shader))
(defun make-shader (vertex-code fragment-code)
  "Create a shader using the glsl source code supplied
for the vertex and fragment shader.
Must be manually freed by calling DELETE-GL"
  (let ((vert (compile-shader :vertex-shader vertex-code))
	(frag (compile-shader :fragment-shader fragment-code))
	(program (gl:create-program)))
    ;; link shader
    (gl:attach-shader program vert)
    (gl:attach-shader program frag)
    (gl:link-program program)
    (let ((log (gl:get-program-info-log program)))
      (if (not (eql (length log) 0))
	  (error 'shader-link-error :link-log log
		 :vertex-code vertex-code :fragment-code fragment-code)))
    (gl:detach-shader program vert)
    (gl:detach-shader program frag)
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    (create-gl)
    (let ((shader (make-instance 'shader :id program))) shader)))

(defmethod delete-gl ((obj shader))
  (gl:delete-program (id obj))
  (call-next-method))

(defmethod bind-gl ((obj shader))
  (gl:use-program (id obj)))

(defparameter *shader-warnings* nil
	      "Track which shader variable warnings have already appeared")

(declaim (ftype (function (shader string) integer) shader-loc))
(defun shader-loc (shader name)
  "Get location of the uniform with name in the shader"
  (let ((location (gl:get-uniform-location (id shader) name)))
    (cond ((and (= location -1)
		(not (find name (cdr (assoc (id shader) *shader-warnings*)))))
	   (warn "Shader ~a ~%did not have a variable with name ~a. (The variable may not have been used)" shader name)
	   (if (assoc (id shader) *shader-warnings*)
	       (setf (cdr (assoc (id shader) *shader-warnings*))
		     (cons name (cdr (assoc (id shader) *shader-warnings*))))
	     (push (list (id shader) name) *shader-warnings*))))
    location))

(defun check-shader-bound (shader)
  (equalp (id shader) (gl:get-integer :current-program)))

(defun unbound-shader-error-text (shader)
  (if (check-shader-bound shader) ""
    (format nil "Passed shader is not bound. Call (BIND-GL shader) before setting shader uniform values.~%~%")))

;;; ------- Helpers -------

(declaim (ftype (function (shader-type string)) compile-shader))
(defun compile-shader (shader-type source-code)
  (let ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader source-code)
    (gl:compile-shader shader)
    (let ((compile-log (gl:get-shader-info-log shader)))
      (if (not (eql (length compile-log) 0))
	  (error 'shader-compile-error :compile-log compile-log :source source-code)))
    shader))


(define-condition shader-compile-error (error)
		  ((compile-log :initarg :compile-log :reader compile-log)
		   (source :initarg :source :reader source))
		  (:report (lambda (condition stream)
			     (format stream "The shader with code~%~a~% could not be compiled~%~a"
				     (source condition) (compile-log condition)))))

(define-condition shader-link-error (error)
  ((link-log :initarg :link-log :reader link-log)
   (vertex-code :initarg :vertex-code :reader vertex-code)
   (fragment-code :initarg :fragment-code :reader fragment-code))
  (:report (lambda (condition stream)
	     (format stream "The shader program with 
vertex shader code: ~%\"~a\"~%
fragment shader code: ~%\"~a\"~% 
could not be linked~%~a"
		     (vertex-code condition) (fragment-code condition) (link-log condition)))))
