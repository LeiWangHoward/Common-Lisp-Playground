;;(load "/Users/leiwang/Documents/courses/cs325/graham-exercise/intersect-segments")

(defun intersect-segments (x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((k1 (slope x1 y1 x2 y2))
        (k2 (slope x3 y3 x4 y4)))
  (cond ((is-point x1 y1 x2 y2)
         (point-intersect x1 y1 x3 y3 x4 y4))
        ((is-point x3 y3 x4 y4)
         (point-intersect x3 y3 x1 y1 x2 y2))
        ((judge-parallel x1 y1 x2 y2 x3 y3 x4 y4)
         (parallel-intersect k1 x1 y1 x2 y2 x3 y3 x4 y4))
        (t (nonparallel-intersect k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)))))

(defun slope (x1 y1 x2 y2)
  (if (= x1 x2)
      most-positive-fixnum
    (/ (- y2 y1) (- x2 x1))))

(defun getb(x1 y1 x2 y2)
  (if (= x1 x2)
      most-negative-fixnum
    (+ y1 (/ (* x1 (- y1 y2))(- x2 x1)))))

(defun is-point (x1 y1 x2 y2)
  (if (and (= x1 x2) (= y1 y2))
      t
    nil))

(defun point-intersect (x y x1 y1 x2 y2)
  (cond ((and (is-point x1 y1 x2 y2) (is-point x y x1 y1))
         (values x y x y)) 
        ((on-segment x y x1 y1 x2 y2)
         (values x y x y))
        (t nil)))

(defun on-segment (x y x1 y1 x2 y2)
  (if (and (eql y (+ (* x (slope x1 y1 x2 y2))
                     (getb x1 y1 x2 y2)))
           (>= x (min x1 x2))
           (<= x (max x1 x2)))
      t
    nil))

(defun judge-parallel (x1 y1 x2 y2 x3 y3 x4 y4)
  (if (= (slope x1 y1 x2 y2) (slope x3 y3 x4 y4))
      t
    nil))

(defun parallel-intersect (k1 x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((and (= k1 most-positive-fixnum)
              (= x1 x3))
         (intersect-x x1 y1 y2 y3 y4))
        ((and (zerop k1) 
              (= y1 y3))
         (intersect-y y1 x1 x2 x3 x4))
        ((eql (getb x1 y1 x2 y2)
              (getb x3 y3 x4 y4))
         (intersect k1 x1 y1 x2 y2 x3 y3 x4 y4))
        (t nil)))

(defun intersect-x (x a1 a2 b1 b2)
  (when (and (>= (max b1 b2) (min a1 a2))
             (>= (max a1 a2) (min b1 b2)))
    (values x (min (max a1 a2) (max b1 b2))
            x (max (min a1 a2) (min b1 b2)))))

(defun intersect-y (y a1 a2 b1 b2)
  (when (and (>= (max b1 b2) (min a1 a2))
             (>= (max a1 a2) (min b1 b2)))
    (values (min (max a1 a2) (max b1 b2)) y
            (max (min a1 a2) (min b1 b2)) y)))

(defun intersect (k1 x1 y1 x2 y2 x3 y3 x4 y4)
  (when (and (>= (max x1 x2) (min x3 x4))
             (>= (max x3 x4) (min x1 x2)))
    (if (> k1 0)
        (positive-slope x1 y1 x2 y2 x3 y3 x4 y4)
      (negative-slope x1 y1 x2 y2 x3 y3 x4 y4))))

(defun positive-slope (x1 y1 x2 y2 x3 y3 x4 y4)
  (values (max (min x1 x2) (min x3 x4))
          (max (min y1 y2) (min y3 y4))
          (min (max x1 x2) (max x3 x4))
          (min (max y1 y2) (max y3 y4))))

(defun negative-slope (x1 y1 x2 y2 x3 y3 x4 y4)
  (values (max (min x1 x2) (min x3 x4))
          (min (max y1 y2) (max y3 y4))
          (min (max x1 x2) (max x3 x4))
          (max (min y1 y2) (min y3 y4))))

(defun nonparallel-intersect (k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)
  (cond ((and (/= k1 most-positive-fixnum)
              (/= k2 most-positive-fixnum))
         (intersect-point k1 k2 x1 y1 x2 y2 x3 y3 x4 y4))
        ((= k1 most-positive-fixnum)
         (intersect-y-parallel x1 x3 y3 x4 y4))
        ((= k2 most-positive-fixnum)
         (intersect-y-parallel x3 x1 y1 x2 y2))
        (t nil)))
  

(defun intersect-point (k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)
  (let* ((b1 (getb x1 y1 x2 y2))
         (b2 (getb x3 y3 x4 y4))
         (x (/ (- b2 b1) (- k1 k2)))
         (y (/ (-(* b1 k2) (* b2 k1)) (- k2 k1))))
    (if (and (on-segment x y x1 y1 x2 y2)
             (on-segment x y x3 y3 x4 y4))
        (values x y)
      nil)))

(defun intersect-y-parallel (x x1 y1 x2 y2)
  (let ( (y (+ (* (/ (- x x1) (- x2 x1)) (- y2 y1)) y1)))
    (if (and (>= y (min y1 y2))
               (<= y (max y1 y2)))
        (values x y)
      nil)))
