(in-package :gficl)

(defparameter *state* nil
  "store internal state of render")

(defun closed-p ()
  "check if the window is to be closed"
  (glfw:window-should-close-p))

(defun window-width ()
  (win-width *state*))

(defun window-height ()
  (win-height *state*))

(defmacro with-update ((frame-time-var) &body body)
  "poll input and window events, frame time var
gives the seconds since last update"
  `(progn
     (let ((,frame-time-var (update-frame-time)))
       (glfw:poll-events)
       ,@body)))


(defmacro with-render (&body body)
  "swaps the backbuffer at end, clears colour-buffer at start."
  `(progn
     ,@body
     (glfw:swap-buffers)))

(defmacro macro-check-type (arg type env)
  `(if (constantp ,arg ,env)
       (check-type ,arg ,type)
     (if (symbolp ,arg)
	 `(check-type ,,arg ,',type)
       `(let ((val ,,arg))
	  (check-type val ,',type)))))

(defmacro with-window
    ((&key
      (title "window")
      (width 500)
      (height 300)
      (visible t)
      (clear-colour '(make-colour 0 0 0 0))
      (cursor :normal) ;; normal, hidden, disabled
      (vsync t)
      (opengl-version-major 3)
      (opengl-version-minor 3))
     &body body &environment env)
  `(progn
     ,(macro-check-type title string env)
     ,(macro-check-type width integer env)
     ,(macro-check-type height integer env)
     ,(macro-check-type clear-colour colour env)
     ,(macro-check-type cursor cursor-state env)
     (setf *state* (make-instance 'render-state :width ,width :height ,height))
     ;; keys found in glfw:create-window
     (glfw:with-init-window
      (:title ,title :width ,width :height ,height :visible ,visible
	      :context-version-major ,opengl-version-major
	      :context-version-minor ,opengl-version-minor)
      (register-glfw-callbacks)
      (glfw:set-input-mode :cursor ,cursor)
      (pass-colour gl:clear-color ,clear-colour)
      (glfw:swap-interval (if ,vsync 1 0))
      ,@body)))


;;; --- Helpers ---


(defclass render-state ()
  ((width :initarg :width :accessor win-width :type integer)
   (height :initarg :height :accessor win-height :type integer)
   (frame-time :initform (get-internal-real-time) :accessor frame-time :type integer)
   (prev-frame-time :initform (get-internal-real-time) :accessor prev-frame-time :type integer))
  (:documentation "stores interal state of the renderer"))


(defun update-frame-time ()
  (let ((dt 0))
    (setf (frame-time *state*) (get-internal-real-time))
    (setf dt (/ (- (frame-time *state*) (prev-frame-time *state*))
		internal-time-units-per-second))
    (setf (prev-frame-time *state*) (frame-time *state*))
    dt))


;;; ------------- GLFW CALLBACKS -----------------

(defun register-glfw-callbacks ()
  ;; set opengl pointer to lib loaded by glfw
  (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address) 
  (glfw:set-key-callback 'quit-with-esc)
  (glfw:set-window-size-callback 'resize-callback))


(glfw:def-key-callback quit-with-esc (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))


(glfw:def-window-size-callback resize-callback (window w h)
  (declare (ignore window))
  (setf (win-width *state*) w)
  (setf (win-height *state*) h))
