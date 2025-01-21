(in-package :gficl)

(defun closedp ()
  "return T if the window should be closed, returns NIL otherwise."
  (glfw:window-should-close-p))

(defun window-width ()
  "returns the current window width"
  (win-width *state*))

(defun window-height ()
  "returns the current window height"
  (win-height *state*))

(defun toggle-fullscreen (&optional (windowed-borderless nil))
  "Changes the window from windowed to fullscreen or fullscreen to windowed
depending on it's current state.
If windowed-borderless is true then the fullscreen mode will be windowed without borders"
  (set-fullscreen (not (fullscreen *state*)) windowed-borderless))

(defmacro with-update ((&optional frame-time-var) &body body)
  "Polls input and window events. 
frame-time-var gives the seconds since last update"
  (if frame-time-var
      `(let ((,frame-time-var (update-frame-time))) (update-render-state) (glfw:poll-events) ,@body)
    `(progn (glfw:poll-events) ,@body (update-render-state))))

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
      (resize-callback '(lambda (w h) (declare (ignore w h))))
      (pre-window-fn '(lambda () ())))
     &body body &environment env)
  "Open a glfw window in the body of this function. 
RESIZE-CALLBACK is only called when width and height are non-zero.
PRE-WINDOW-FN is called after glfw is initialised but before a window is created."
  `(progn
     ,(macro-check-type title string env)
     ,(macro-check-type width integer env)
     ,(macro-check-type height integer env)
     ,(macro-check-type cursor cursor-state env)
     (setf *state*
	   (make-instance 'render-state :width ,width :height ,height :resize-fn ,resize-callback))
     (setf *active-objects* 0)
     (setf *shader-warnings* nil)
     ;; keys found in glfw:create-window
     (glfw:with-init
      (funcall ,pre-window-fn)
      (glfw:with-window 
       (:title ,title :width ,width :height ,height :visible ,visible
	       :context-version-major ,opengl-version-major
	       :context-version-minor ,opengl-version-minor )
       (register-glfw-callbacks)
       (glfw:set-input-mode :cursor ,cursor)
       (glfw:swap-interval (if ,vsync 1 0))
       ,@body
       (if (not (= 0 *active-objects*))
	   (format t "~%Warning: ~a gl object~:p ~:*~[ ~;was~:;were~] not freed~%"
		   *active-objects*))))))

(declaim (ftype (function (boolean boolean)) set-fullscreen))
(defun set-fullscreen (bool windowed-borderless)
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
    (%glfw:set-window-monitor
     glfw:*window*
     (if windowed-borderless (cffi:null-pointer) monitor)
     x y width height refresh)
    (setf (fullscreen *state*) bool)))

;;; ------------- GLFW CALLBACKS -----------------

(defun register-glfw-callbacks ()
  ;; set opengl pointer to lib loaded by glfw
  (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address) 
  (glfw:set-key-callback 'quit-with-esc)
  (glfw:set-window-size-callback 'resize-callback))

(glfw:def-key-callback quit-with-esc (window key scancode action mod-keys)
  (declare (ignore window scancode mod-keys))
  (update-key-state (render-input *state*) key action))

(glfw:def-window-size-callback resize-callback (window w h)
  (declare (ignore window))
  (setf (win-width *state*) w)
  (setf (win-height *state*) h)
  (gl:viewport 0 0 w h)
  (if (not (or (= w 0) (= h 0)))
      (funcall (resize-fn *state*) w h)))
