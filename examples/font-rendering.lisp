
(defparameter letter
   (truetype-clx:text-pixarray #p"examples/assets/Roboto-Black.ttf" "A" 50 100 100))

(defparameter tex nil)

(gficl:with-window
 (:title "font" :width 600 :height 400)
 (gl:clear-color 0 1 0 0)
 (destructuring-bind (w h) (array-dimensions letter)
    (let ((data (cffi:foreign-alloc :unsigned-char :initial-element 255 :count (* w h 4))))
      (loop for x from 0 to w do
	    (loop for y from 0 to h do
		  (setf (cffi:mem-aref data :unsigned-char (+ (* y w 4) (* x 4) 3))
			(aref letter y x))))
      (setf tex (gficl:make-texture w h))
      (cffi:foreign-free data)))
 (loop until (gficl:closed-p)
       do (gficl:with-update ())
       do (gficl:with-render
	   (gl:clear :color-buffer)))
 (gficl:delete-gl tex))
