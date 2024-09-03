(in-package :gficl)

(defun key-down (key)
  "return T if key is currently pressed down, returns NIL otherwise."
  (key-present (render-input *state*) key))

(defun key-pressed (key)
  "return T if the key was up last frame and down this frame, returns NIL otherwise."
  (and (key-down key) (not (key-present (render-prev-input *state*) key))))

(defmacro map-keys-down (&body cases)
  "Take a series of keys and bodies, such as:
(map-keys (:a (fn)) (:b (fn1 x) (fn2)))
Runs any expressions which have their key down."
  `(progn ,@(loop for c in cases collecting `(if (key-down ,(car c)) (progn ,@(cdr c))))))

(defmacro map-keys-pressed (&body cases)
  "Take a series of keys and expressions, such as:
(map-keys (:a (fn)) (:b (fn1 x) (fn2)))
Runs any expressions which have their key pressed (key down this frame, key up last frame)."
  `(progn ,@(loop for c in cases collecting `(if (key-pressed ,(car c)) (progn ,@(cdr c))))))

;; --- helpers ---

(defun key-present (state key)
  "check if key is present in state hash table"
  (nth-value 1 (gethash key (slot-value state 'key-state))))
