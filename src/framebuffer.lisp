(in-package :gficl)

(defclass framebuffer (gl-object)
  ((attachments :initform (list))))

(defun create-framebuffer ())

(deftype attachment-pos ()
  '(member  :color-attachment0 :depth-attachment :stencil-attachment :depth-stencil-attachment))

(defclass attachment (gl-object)
  ((attach-pos :initarg :position :accessor attach-pos :type attachment-pos)))

(defun make-attachment (position format)
  (declare (attachment-pos position) (image-format format))
  (make-instance 'attachment :position position))

(defmethod delete-gl ((obj attachment)))
