;;v 1.0 not working
(defun delete-car (lst)
  (lambda (x)
    (if (consp x)
        (setf lst (rest x)))))
;;v 2.0 extra ()
(defun delete-car (lst)
  ;(let ((y (rest x)))
  (rplaca lst (value-list (cdr lst)))
  (rplacd lst nil))

;;v 3.0
(defun delete-car (lst)
  (cond ((not (null (cdr lst)))
         (rplaca lst (car (cdr lst)))
         (rplacd lst (cdr (cdr lst))))
        (t nil)))

