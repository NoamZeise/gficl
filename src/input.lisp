(in-package :gficl)

(defmacro if-key (key if &optional then)
  "execute if when key is pressed, otherwise execute else"
  `(if (equalp (glfw:get-key ,key) :press)
       ,if ,then))

(defmacro map-keys (cases)
  "take a list of keys and expressions. ie. (map-keys ((:a (fn)) (:b (fn))))
runs any expressions which have their key pressed."
  `(progn ,@(loop for c in cases collecting
		  `(if-key ,(car c) ,(cadr c)))))
