(defun make-change (payment &optional (coins '(25 10 5 1)))
  (when coins
    (multiple-value-bind (num-coin owe-pay) (floor payment (car coins))
      ;(multiple-value-list num-coin (make-change own-pay (cdr coins)))))
      (cons num-coin (make-change owe-pay (cdr coins)))))
  ;(value-list num-coin)
)

#|
(defun make-change (payment &optional (coins '(25 10 5 1)))
  (let ((change '(1 2))
        (owe-pay payment))
    (dolist (coin coins)
       (multiple-value-bind (num-coin owe-pay) (floor owe-pay coin)
         (append change (list num-coin))))
    (values-list change)))
|#

#|
(defun make-change (payment &optional (coins '(25 10 5 1)))
  (do ((change nil)
       (owe-pay payment)
       (coin coins (rest coin)))
      ((null coin) (apply #'values change)) ;values-list
     (multiple-value-bind (num-coin owe-pay) (floor owe-pay (car coin))
       (cons num-coin change))))|#
#|
(defun make-change (payment &optional (coins '(25 10 5 1)))
  (when coins
    (multiple-value-bind (num-coin owe-pay) (floor payment (car coins))
      (values num-coin (make-change owe-pay (cdr coins)))))
)|#

(defun make-change(payment &optional (coins '(25 10 5 1)))
  (do ((coin coins (rest coin))
       (owe-pay payment (mod owe-pay (car coin)))
       (result nil (append result
                          (list (floor owe-pay (car coin))))))
      ((null coin) (values-list result))))
