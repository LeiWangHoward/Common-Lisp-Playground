(defun make-balance (balance-init)
  (function (lambda (x)
              (+ x balance-init)))

;v1.0
(defun make-balance (balance-init)
  (let ((balance balance-init))
    (lambda (&optional x)
      (if x
          (incf balance x)
        balance))))
;v2.0
(defun make-balance (balance)
  (function (lambda (&optional x)
              (if x
                  (incf balance x)
                balance))))

;v3.0
(defun make-balance (balance)
  (lambda (&optional x)
              (if x
                  (incf balance x)
                balance)))
