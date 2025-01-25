(in-package :gficl)

(defparameter *state* nil
   "store internal state of render")

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
   (fullscreen :initform nil :accessor fullscreen :type boolean)
   (input :initform (make-instance 'input-state) :accessor render-input)
   (prev-input :initform (make-instance 'input-state) :accessor render-prev-input))
  (:documentation "stores interal state of the renderer"))

(defun update-render-state ()
  (update-input-state))

(defun update-frame-time ()
  (let ((dt 0))
    (setf (frame-time *state*) (get-internal-real-time))
    (setf dt (/ (- (frame-time *state*) (prev-frame-time *state*))
		internal-time-units-per-second))
    (setf (prev-frame-time *state*) (frame-time *state*))
    dt))

;; --- input state ---

(defun update-input-state ()
  (setf (slot-value (render-prev-input *state*) 'key-state)
	(alexandria:copy-hash-table (slot-value (render-input *state*) 'key-state)))
  (setf (slot-value (render-prev-input *state*) 'mouse-state)
	(alexandria:copy-hash-table (slot-value (render-input *state*) 'mouse-state))))

(defclass input-state ()
  ((key-state :initform (make-hash-table) :type hash-table)
   (mouse-state :initform (make-hash-table) :type hash-table))
  (:documentation "store previous frame's pressed keys."))

(defgeneric update-key-state (state key action)
  (:documentation "handle a key input state change."))

(defmethod update-key-state ((state input-state) key action)
  (case action
	(:press (setf (gethash key (slot-value state 'key-state)) t))
	(:release (remhash key (slot-value state 'key-state)))))

(defgeneric update-mouse-pos (state x y)
  (:documentation "handle a mouse position change"))

(defmethod update-mouse-pos ((state input-state) x y)
  (with-slots (mouse-state) state
    (setf (gethash :x mouse-state) x)
    (setf (gethash :y mouse-state) y)))

(defgeneric update-mouse-buttons (state button action)
  (:documentation "handle a mouse button state change."))

(defmethod update-mouse-buttons ((state input-state) button action)
  (with-slots (mouse-state) state
    (case action
	  (:press (setf (gethash button mouse-state) t))
	  (:release (remhash button mouse-state)))))

;; --- hardware state ---

(defparameter *max-msaa-samples* 0)

(declaim (ftype (function (&optional integer) integer) msaa-samples))
(defun msaa-samples (&optional (samples 0))
  "Return the minimum of SAMPLES and maximum supported samples.
Returns maximum supported samples if SAMPLES is 0, or samples isn't passed"
  (if (= *max-msaa-samples* 0)
      (setf *max-msaa-samples* (gl:get-integer :max-samples)))
  (if (> samples 0) (min samples *max-msaa-samples*) *max-msaa-samples*))
