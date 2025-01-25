(in-package :gficl)

;; Keyboard Keys

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

;; Mouse Buttons

(defun button-down (button)
  "return T if button is currently pressed down, returns NIL otherwise."
  (button-present (render-input *state*) button))

(defun button-pressed (button)
  "return T if the button was up last frame and down this frame, returns NIL otherwise."
  (and (button-down button) (not (button-present (render-prev-input *state*) button))))

(defmacro map-buttons-down (&body cases)
  "Take a series of buttons and bodies, such as:
(map-buttons (:a (fn)) (:b (fn1 x) (fn2)))
Runs any expressions which have their button down."
  `(progn ,@(loop for c in cases collecting `(if (button-down ,(car c)) (progn ,@(cdr c))))))

(defmacro map-buttons-pressed (&body cases)
  "Take a series of buttons and expressions, such as:
(map-buttons (:a (fn)) (:b (fn1 x) (fn2)))
Runs any expressions which have their button pressed (button down this frame, button up last frame)."
  `(progn ,@(loop for c in cases collecting `(if (button-pressed ,(car c)) (progn ,@(cdr c))))))

;; Mouse Position

(defun mouse-pos (&optional as-ndc)
  "return the mouse pos as '(x y).
if AS-NDC is true, will be in the range [-1, 1].
Otherwise return as window pixel values."
  (with-slots (mouse-state) (render-input *state*)
    (let ((x (gethash :x mouse-state))
	  (y (gethash :y mouse-state)))
      (flet ((to-ndc (val range) (if as-ndc (- (/ (* 2 val) range) 1) val)))
	    (list (to-ndc (if x x 0) (window-width))
		  (to-ndc (if y y 0) (window-height)))))))

;; --- helpers ---

(defun key-present (state key)
  "check if key is present in state hash table"
  (nth-value 1 (gethash key (slot-value state 'key-state))))

(defun button-present (state button)
  "check if key is present in state hash table"
  (nth-value 1 (gethash button (slot-value state 'mouse-state))))
