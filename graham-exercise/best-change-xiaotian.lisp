;;make-best-change
(defun make-best-change (total  &optional coins)
  (let ((changes (if coins coins '(25 10 5 1))))
    (values-list (reverse (car (best-change-helper total changes))))))

(defun best-change-helper (total changes &optional (result nil) (best nil))
  (do ((lst (get-coin-set total changes) (cdr lst))
       (best-opt best 
                 (best-change-helper (- total (* (car changes) (car lst)))
                                     (cdr changes)
                                     (cons (car lst) result)
                                     best-opt)))
      ((null lst) (get-result total result best-opt))))


(defun get-coin-set (total changes)
  (cond ((null changes) nil)
        ((null (cdr changes)) (list (floor total (car changes))))
        (t (make-coin-list total changes))))

(defun make-coin-list (total changes)
  (let* ((temp1 (+ 1 (floor total (car changes))))
         (temp2 temp1))
    (mapcar #'(lambda (x) (decf temp2)) (make-list temp1))))
       
                                       
(defun get-result (total result best-opt)
  (cond ((null best-opt) (cons result total))
        ((< total (cdr best-opt)) (cons result total))
        ((and (= total (cdr best-opt))
               (< (reduce #'+ result) (reduce #'+ (car best-opt))))
         (cons result total))
        (t best-opt)))                                 
