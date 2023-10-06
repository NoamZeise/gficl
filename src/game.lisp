(in-package :gficl)

(defun game-closed-p ()
  (glfw:window-should-close-p))

(defun set-game-should-close ()
  (glfw:set-window-should-close))

(defmacro with-update (&body body)
  `(progn
     (glfw:poll-events)
     ,@body))

(defmacro with-render (&body body)
  `(progn
     (gl:clear :color-buffer)
     ,@body
     (glfw:swap-buffers)))


(defmacro macro-check-type (arg type env)
  `(if (constantp ,arg ,env)
       (check-type ,arg ,type)
       (if (symbolp ,arg)
	   `(check-type ,,arg ,',type)
	   `(let ((val ,,arg))
	      (check-type val ,',type)))))

(defmacro with-game
    ((&key
	(title "Game")
	(width 500)
	(height 300)
	(visible t)
	(clear-colour '(make-colour 0 0 0 0))
	(cursor :normal)
	(vsync t)) ;; normal, hidden, disabled
     &body body &environment env)
  `(progn
     ,(macro-check-type title string env)
     ,(macro-check-type width integer env)
     ,(macro-check-type height integer env)
     ,(macro-check-type clear-colour colour env)
     ,(macro-check-type cursor cursor-state env)
     ;; keys found in glfw:create-window
     (glfw:with-init-window (:title ,title :width ,width :height ,height :visible ,visible)
       (register-glfw-callbacks)
       (glfw:set-input-mode :cursor ,cursor)
       (pass-colour gl:clear-color ,clear-colour)
       (glfw:swap-interval (if ,vsync 1 0))
       (set-gl-viewport ,width ,height)
       ,@body)))
