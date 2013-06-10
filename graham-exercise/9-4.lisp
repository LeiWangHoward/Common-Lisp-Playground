;;(load "/Users/leiwang/Documents/courses/cs325/graham-exercise/9-4")

(defun intersect-segments(x1 y1 x2 y2 x3 y3 x4 y4)
  (let ((s1 (slope  x1 y1 x2 y2))
        (s2 (slope  x3 y3 x4 y4)))
  (cond((judge-parallel x1 y1 x2 y2 x3 y3 x4 y4)
        (work-parallel k1 x1 y1 x2 y2 x3 y3 x4 y4))
        ((is-point x1 y1 x2 y2)
         (report-point x1 y1 x3 y3 x4 y4))
        ((is-point x3 y3 x4 y4)
         (report-point x3 y3 x1 y1 x2 y2))
        (t(work-not-parallel k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)))))
(defun slope (x1 y1 x2 y2)
  (cond ((= x1 x2) 'y)
        ((= y1 y2) 'x)
        (t (/ (- y2 y1) (- x2 x1)))))

(defun judge-parallel (x1 y1 x2 y2 x3 y3 x4 y4)
  (if (= (slope x1 y1 x2 y2) (slope x3 y3 x4 y4))
      t
    nil))

;(defun parallel-overlap (x1 y1 x2 y2 x3 y3 x4 y4)
;  (cond ((> x1 x3)


(defun work-not-parallel(k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)
  (cond((and(/= k1 most-positive-fixnum)
             (/= k2 most-positive-fixnum))
        (find-point k1 k2 x1 y1 x2 y2 x3 y3 x4 y4))
        ((= k1 most-positive-fixnum)
         (report x1 x3 y3 x4 y4))
        ((= k2 most-positive-fixnum)
         (report x3 x1 y1 x2 y2))
        (t nil)))
  
(defun work-parallel(k1 x1 y1 x2 y2 x3 y3 x4 y4)
  (cond((and(= k1 most-positive-fixnum)
             (= x1 x3))
        (intersect-x x1 y1 y2 y3 y4))
        ((and(zerop k1)(= y1 y3))
         (intersect-y y1 x1 x2 x3 x4))
        ((eql(getb x1 y1 x2 y2)
              (getb x3 y3 x4 y4))
         (intersect k1 x1 y1 x2 y2 x3 y3 x4 y4))
        (t nil)))

(defun is-point(x1 y1 x2 y2)
  (and(= x1 x2)(= y1 y2)))
    
(defun report-point(x y x1 y1 x2 y2)
  (when(point-on-segment x y x1 y1 x2 y2)
    (values x y x y)))

(defun find-point(k1 k2 x1 y1 x2 y2 x3 y3 x4 y4)
  (let*((b1(getb x1 y1 x2 y2))
        (b2(getb x3 y3 x4 y4))
        (x(/(- b2 b1)(- k1 k2)))
        (y(/(-(* b1 k2)(* b2 k1))(- k2 k1))))
    (when(and(point-on-segment x y x1 y1 x2 y2)
              (point-on-segment x y x3 y3 x4 y4))
      (values x y))))

(defun report(x x1 y1 x2 y2)
  (let((y (+(*(/(- x x1)(- x2 x1))(- y2 y1))y1)))
    (when(and(>= y(min y1 y2))
              (<= y(max y1 y2)))
        (values x y))))
  
                            
(defun intersect-x(x a1 a2 b1 b2)
  (when(and(>=(max b1 b2)(min a1 a2))
          (>=(max a1 a2)(min b1 b2)))
    (values x(min(max a1 a2)(max b1 b2))
            x(max(min a1 a2)(min b1 b2)))))

(defun intersect-y(y a1 a2 b1 b2)
  (when(and(>=(max b1 b2)(min a1 a2))
          (>=(max a1 a2)(min b1 b2)))
    (values(min(max a1 a2)(max b1 b2))y
            (max(min a1 a2)(min b1 b2))y)))

(defun intersect(k1 x1 y1 x2 y2 x3 y3 x4 y4)
  (when(and(>=(max x1 x2)(min x3 x4))
            (>=(max x3 x4)(min x1 x2)))
    (if(> k1 0)
        (values1 x1 y1 x2 y2 x3 y3 x4 y4)
      (values2 x1 y1 x2 y2 x3 y3 x4 y4))))

(defun values1(x1 y1 x2 y2 x3 y3 x4 y4)
  (values (max(min x1 x2)(min x3 x4))
          (max(min y1 y2)(min y3 y4))
          (min(max x1 x2)(max x3 x4))
          (min(max y1 y2)(max y3 y4))))

(defun values2(x1 y1 x2 y2 x3 y3 x4 y4)
  (values (max(min x1 x2)(min x3 x4))
          (min(max y1 y2)(max y3 y4))
          (min(max x1 x2)(max x3 x4))
          (max(min y1 y2)(min y3 y4))))

     ; most-positive-fixnum

(defun getb(x1 y1 x2 y2)
  (if(= x1 x2)
      most-negative-fixnum
    (- y1 (/ (* x1(- y2 y1))(- x2 x1)))))

(defun point-on-segment(x y x1 y1 x2 y2)
  (and(eql y
           (+(* x(getk x1 y1 x2 y2))
              (getb x1 y1 x2 y2)))
       (>= x (min x1 x2))
       (<= x (max x1 x2))))

;(defun distance (x1 y1 x2 y2)
;  (let ((dx (- x2 x1))
;        (dy (- y2 y1)))))
