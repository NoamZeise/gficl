(in-package :gficl)

(defun game-closed-p ()
  (glfw:window-should-close-p))

(defmacro with-update (&body body)
  `(progn
     (glfw:poll-events)
     ,@body))

(defmacro with-render (&body body)
  `(progn
     (gl:clear :color-buffer)
     ,@body
     (glfw:swap-buffers)))

(deftype cursor-state () '(member :normal :hidden :disabled))

(defmacro with-game
    ((&key
	(title "Game")
	(width 500)
	(height 300)
	(visible t)
	(clear-colour '(make-colour 0 0 0 0))
	(cursor :normal)) ;; normal, hidden, disabled
     &body body)
    (declare (type cursor-state cursor)
	     (type string title))
  (if (not (eql (type-of (eval clear-colour)) 'colour))
      (error "The value of clear-colour was not of type COLOUR!"))
  (setf width  (max 1 (abs (round width))))
  (setf height (max 1 (abs (round height))))
  `(trivial-main-thread:with-body-in-main-thread ()
     ;; keys found in glfw:create-window
     (glfw:with-init-window (:title ,title :width ,width :height ,height :visible ,visible)
       ;; set opengl pointer to lib loaded by glfw
       (setf %gl:*gl-get-proc-address* #'glfw:get-proc-address) 
       (glfw:set-key-callback 'quit-with-esc)
       (glfw:set-window-size-callback 'resize-callback)
       (glfw:set-input-mode :cursor ,cursor)
       (pass-colour gl:clear-color ,clear-colour)
       (set-gl-viewport ,width ,height)
       ,@body)))
