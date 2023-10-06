(in-package :gficl)

(defun register-glfw-callbacks ()
  ;; set opengl pointer to lib loaded by glfw
  (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address) 
  (glfw:set-key-callback 'quit-with-esc)
  (glfw:set-window-size-callback 'resize-callback))

(glfw:def-key-callback quit-with-esc (window key scancode action mod-keys)
  (declare (ignore scancode mod-keys))
  (when (and (eq key :escape) (eq action :press))
    (glfw:set-window-should-close)))

(defun set-gl-viewport (w h)
  (gl:viewport 0 0 w h)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 1 0 1 -1 1))

(glfw:def-window-size-callback resize-callback (window w h)
  (declare (ignore window))
  (set-gl-viewport w h))
