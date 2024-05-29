(in-package :gficl)

(defparameter *state* nil
  "store internal state of render")

(defun closed-p ()
  "check if the window is to be closed"
  (glfw:window-should-close-p))

(defun window-width ()
  "get the current window width"
  (win-width *state*))

(defun window-height ()
  "get the current window height"
  (win-height *state*))

(defun toggle-fullscreen ()
  (set-fullscreen (not (fullscreen *state*))))

(defmacro with-update ((frame-time-var) &body body)
  "poll input and window events. frame-time-var gives the seconds since last update"
  `(progn
     (let ((,frame-time-var (update-frame-time)))
       (glfw:poll-events)
       ,@body)))

(defmacro with-render (&body body)
  "enclose gl render calls, swaps the backbuffer at end."
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
      (cursor :normal) ;; normal, hidden, disabled
      (vsync t)
      (opengl-version-major 3)
      (opengl-version-minor 3)
      (resize-callback (lambda (w h) (declare (ignore w h)))))
     &body body &environment env)
  "Open a glfw window in the body of this function. 
RESIZE-CALLBACK is only called when width and height are non-zero."
  `(progn
     ,(macro-check-type title string env)
     ,(macro-check-type width integer env)
     ,(macro-check-type height integer env)
     ,(macro-check-type cursor cursor-state env)
     (setf *state*
	   (make-instance 'render-state :width ,width :height ,height :resize-fn ,resize-callback))
     (setf *active-objects* 0)
     ;; keys found in glfw:create-window
     (glfw:with-init-window
      (:title ,title :width ,width :height ,height :visible ,visible
	      :context-version-major ,opengl-version-major
	      :context-version-minor ,opengl-version-minor)
      (register-glfw-callbacks)
      (glfw:set-input-mode :cursor ,cursor)
      (glfw:swap-interval (if ,vsync 1 0))
      ,@body
      (if (not (= 0 *active-objects*))
	  (format t "~%Warning: ~a gl object~:p ~:*~[ ~;was~:;were~] not freed~%"
		  *active-objects*)))))

(declaim (ftype (function (boolean)) set-fullscreen))
(defun set-fullscreen (bool)
  (if (not (fullscreen *state*))
      (progn (setf (prev-width *state*) (window-width))
	     (setf (prev-height *state*) (window-height))
	     (destructuring-bind (x y) (glfw:get-window-position)
	       (setf (prev-x *state*) x)
	       (setf (prev-y *state*) y))))
  (let* ((monitor (if bool (glfw:get-primary-monitor)
		    (cffi:null-pointer)))
	 (mode (if bool (glfw:get-video-mode monitor) nil))
	 (x (if bool 0 (prev-x *state*)))
	 (y (if bool 0 (prev-y *state*)))
	 (width (if bool (getf mode '%glfw::width)
		  (prev-width *state*)))
	 (height (if bool (getf mode '%glfw::height)
		   (prev-height *state*)))
	 (refresh (if bool (getf mode '%glfw::refresh-rate) 0)))
    (%glfw:set-window-monitor glfw:*window*
			      monitor x y width height refresh)
    (setf (fullscreen *state*) bool)))

;;; --- Helpers ---

(defclass render-state ()
  ((width :initarg :width :accessor win-width :type integer)
   (height :initarg :height :accessor win-height :type integer)
   (prev-x :initform 0 :accessor prev-x :type integer)
   (prev-y :initform 0 :accessor prev-y :type integer)
   (prev-width :initform 0 :accessor prev-width :type integer)
   (prev-height :initform 0 :accessor prev-height :type integer)
   (resize-fn :initarg :resize-fn :accessor resize-fn :type (function (integer integer)))
   (frame-time :initform (get-internal-real-time) :accessor frame-time :type integer)
   (prev-frame-time :initform (get-internal-real-time) :accessor prev-frame-time :type integer)
   (fullscreen :initform nil :accessor fullscreen :type boolean))
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
  (declare (ignore scancode mod-keys)))

(glfw:def-window-size-callback resize-callback (window w h)
  (declare (ignore window))
  (setf (win-width *state*) w)
  (setf (win-height *state*) h)
  (if (not (or (= w 0) (= h 0)))
      (funcall (resize-fn *state*) w h)))
