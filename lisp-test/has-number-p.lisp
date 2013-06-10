(defun has-number-p (lst)
  (cond ((atom lst) (numberp lst))
        ((listp lst) (some #'numberp
                          (if (listp (car lst))
                              (car lst)
                            lst)))
        (t (has-number-p (cdr lst)))))
