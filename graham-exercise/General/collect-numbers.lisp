;;v 1.0 
(mapcan (lambda (x) (if (numberp x) (list x) nil)) '(-4 6 -23 1 0 12 a s d 2 4 5))


;;v 1.1 cannot break into multiple "()" and only return one number sometime(find number -> end it) TODO:cons
(defun collect-numbers (lst)
  (cond ((atom lst) (if (numberp lst) 
                        (list lst)
                      nil)) 
        ((listp lst) (mapcan (lambda (x)
                               (if (numberp x) (list x) nil)) 
                             (if (listp (car lst))
                                 (car lst) lst)))
        (t (collect-numbers (cdr lst)))))


;;v 2.0 need combine flatten-list with collect-numbers
(defun collect-numbers (lst)
  (cond ((atom lst) (if (numberp lst) 
                        (list lst)
                      nil)) 
        ((listp lst) (mapcan (lambda (x)
                               (if (numberp x) (list x) nil)) 
                             (flatten-list lst)))))

(defun flatten-list (lst)
  (loop for item in lst
        when (consp item)
        nconc (flatten-list item)
        else
        collect item))

;v 3.0 
(defun collect-numbers (lst)
  (labels ((flatten-list (lst)
             (loop for item in lst
                   when (consp item)
                   nconc (flatten-list item)
                   else
                   collect item)))
    (cond ((atom lst) (if (numberp lst) 
                          (list lst)
                        nil)) 
          (t (mapcan (lambda (x)
                       (if (numberp x) (list x) nil)) 
                     (flatten-list lst))))))

;;v 4.0 without flatten-list
(defun collect-numbers (lst)
  (cond ((null lst) nil)
        ((numberp lst) (list lst))
        ((atom lst) nil)
        (t (mapcan (function collect-numbers) lst))))

;;test cases
(define-test collect-numbers
  (assert-equal nil (collect-numbers nil))
  (assert-equal nil (collect-numbers 'b))
  (assert-equal '(3) (collect-numbers '3))
  (assert-equal '(1 2 3) (collect-numbers '(1 2 3)))
  (assert-equal '(1 2 3) (collect-numbers '(1 (b (2 c) ((3))))))
  (assert-equal '(1 3) (collect-numbers '(1 nil (3 nil))))
  (assert-equal '(4 1 8 2) (collect-numbers '(((4) 1) (8 ((2))))))
  )
