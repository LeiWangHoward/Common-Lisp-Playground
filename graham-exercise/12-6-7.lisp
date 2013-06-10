(setf *print-circle* t)
(defun circular-member-p (obj ls)
  (labels ((rec (ls1)
             (if ls1
                (cond
                   ((eql obj (car ls1)) t)
                   ((eq ls (cdr ls1)) nil)
                   (t (rec (cdr ls1)))))))
    (rec ls)))

(defun cdr-circular-p (ls)
  (labels ((rec (ls1)
             (if ls1
                 (or (eq (cdr ls1) ls)
                     (rec (cdr ls1))))))
    (rec ls)))