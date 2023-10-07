(in-package :gficl)

(defclass framebuffer (gl-object)
  ((attachments :initform (list))))

(defun create-framebuffer ()
  )

(defclass attachment (gl-object)
  ())
