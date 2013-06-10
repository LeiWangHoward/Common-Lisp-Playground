(defclass point ()
  ((x :accessor x
      :initarg :x
      :initform 0)
   (y :accessor y
      :initarg :y
      :initform 0)
   (z :accessor z
      :initarg :z
      :initform 0)))

(defclass surface ()
  ((color  :accessor surface-color
           :initarg :color)))

(defclass sphere (surface)
  ((radius :accessor sphere-radius
           :initarg :radius
           :initform 0)
   (center :accessor sphere-center
           :initarg :center
           :initform (make-instance 'point :x 0 :y 0 :z 0))))

(defun defsphere (x y z r c)
  (let ((s (make-instance  'sphere
                           :radius r
                           :center (make-instance 'point :x x :y y :z z)
                           :color c)))
    (push s *world*)
    s))

(defmethod intersect ((s sphere) (pt point) xr yr zr)
  (let* ((c (sphere-center s))
         (n (minroot (+ (sq xr) (sq yr) (sq zr))
                     (* 2 (+ (* (- (x pt) (x c)) xr)
                             (* (- (y pt) (y c)) yr)
                             (* (- (z pt) (z c)) zr)))
                     (+ (sq (- (x pt) (x c)))
                        (sq (- (y pt) (y c)))
                        (sq (- (z pt) (z c)))
                        (- (sq (sphere-radius s)))))))
    (if n
        (make-instance 'point
                        :x  (+ (x pt) (* n xr))
                        :y  (+ (y pt) (* n yr))
                        :z  (+ (z pt) (* n zr))))))

(defmethod normal ((s sphere) (pt point))
  (let ((c (sphere-center s)))
    (unit-vector (- (x c) (x pt))
                 (- (y c) (y pt))
                 (- (z c) (z pt)))))