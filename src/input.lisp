(in-package :gficl)

(defun key-down (key)
  "return whether key is currently pressed down."
  (key-present (render-input *state*) key))

(defun key-pressed (key)
  "return whether key was just pressed down this frame"
  (and (key-down key) (not (key-present (render-prev-input *state*) key))))

(defmacro map-keys-down (cases)
  "Take a list of keys and expressions. ie. (map-keys ((:a (fn)) (:b (fn))))
runs any expressions which have their key down."
  `(progn ,@(loop for c in cases collecting `(if (key-down ,(car c)) ,(cadr c)))))

(defmacro map-keys-pressed (cases)
  "Take a list of keys and expressions. ie. (map-keys ((:a (fn)) (:b (fn))))
runs any expressions which have their key pressed."
  `(progn ,@(loop for c in cases collecting `(if (key-pressed ,(car c)) ,(cadr c)))))


;; --- helpers ---

(defun key-present (state key)
  "check if key is present in state hash table"
  (nth-value 1 (gethash key (slot-value state 'key-state))))
