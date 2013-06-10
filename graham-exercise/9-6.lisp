(defun horner (x &rest parameters)
  (labels ((divide-solve (parameters acc)
             (if parameters
                 (divide-solve (cdr parameters) (+ (* acc x) (car parameters)))
               acc)))
    (divide-solve parameters 0)))

;;version 2

(defun horner (x-value &rest parameters)
  (reduce #'(lambda (x y) (+ (* x-value x) y)) parameters))