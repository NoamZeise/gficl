(in-package :gficl)

(defclass shader ()
  ((shader-program :initarg :program :initform 0 :accessor shader-program :type integer)))

(declaim (ftype (function (pathname pathname) shader) make-shader))
(defun make-shader (vertex-path fragment-path)
  (let ((vert (compile-shader vertex-path :vertex-shader))
	(frag (compile-shader fragment-path :fragment-shader))
	(program (gl:create-program)))
    (gl:attach-shader program vert)
    (gl:attach-shader program frag)
    (gl:link-program program)
    (let ((log (gl:get-program-info-log program)))
      (if (not (eql (length log) 0))
	  (error 'shader-link-error :link-log log
		 :vertex-path vertex-path :fragment-path fragment-path)))
    (gl:detach-shader program vert)
    (gl:detach-shader program frag)
    (gl:delete-shader vert)
    (gl:delete-shader frag)
    (let ((shader (make-instance 'shader :program program))) shader)))

(declaim (ftype (function (pathname shader-type) integer) compile-shader))
(defun compile-shader (path shader-type)
  (let ((shader (gl:create-shader shader-type))
	 (text (uiop:read-file-string path)))
    (gl:shader-source shader text)
    (gl:compile-shader shader)
    (let ((compile-log (gl:get-shader-info-log shader)))
      (if (not (eql (length compile-log) 0))
	  (error 'shader-compile-error :compile-log compile-log :filename path)))
    shader))

(declaim (ftype (function (shader)) delete-shader))
(defun delete-shader (shader)
  (gl:delete-program (shader-program shader)))

(define-condition shader-compile-error (error)
  ((compile-log :initarg :compile-log :reader compile-log)
   (filename :initarg :filename :reader filename))
  (:report (lambda (condition stream)
	     (format stream "The shader at ~a could not be compiled~%~a"
		     (filename condition) (compile-log condition)))))

(define-condition shader-link-error (error)
  ((link-log :initarg :link-log :reader link-log)
   (vertex-path :initarg :vertex-path :reader vertex-path)
   (fragment-path :initarg :fragment-path :reader fragment-path))
  (:report (lambda (condition stream)
	     (format stream "The shader program with 
vertex shader: \"~a\"
fragment shader: \"~a\" 
could not be linked~%~a"
		     (vertex-path condition) (fragment-path condition) (link-log condition)))))
