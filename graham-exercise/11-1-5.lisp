(defclass rectangle ()
  ((height :accessor rectangle-height
           :initarg :height
           :initform 0)
   (width  :accessor rectangle-width
           :initarg :width
           :initform 0)))

(defclass circle ()
  ((radius :accessor circle-radius
           :initarg :radius
           :initform 0)))

(defmethod area ((x rectangle))
  (* (rectangle-width x) (rectangle-height x)))

(defmethod area ((x circle))
  (let ((*WARN-ON-FLOATING-POINT-CONTAGION* nil)) ;;shut off warning
    (* pi (expt (circle-radius x) 2))))

(defvar *area-counter* 0)
(defmethod area :before (obj)
  (declare (ignore obj))
  (incf *area-counter*))