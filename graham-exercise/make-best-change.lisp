(defun make-best-change (amount &optional (coins '(25 10 5 1)))
  (apply #'values
         (butlast
          (reduce #'(lambda (x y)
                      (cond ((< (car (last x)) (car (last y))) x)
                            ((> (car (last x)) (car (last y))) y)
                            ((< (sum-list (butlast x))
                                (sum-list (butlast y))) x)
                            (t y)))
                  (make-all-change amount coins)))))
;sum elements of a list
(defun sum-list (nums)
  (apply #'+ nums)) ;;reduce
;this returns a list of all the possible changes with
;remaining amount appended at last of each list.
;;version 1.0 TODO: eliminate excessive cons
(defun make-all-change (amount coins)
  (cond ((= amount 0) `(,(make-list 
                       (1+ (length coins)) :initial-element 0)))
        ((null coins) `((,amount)))
        ((< amount (car coins))
         (mapcar #'(lambda (x) (cons 0 x))
                 (make-all-change amount (cdr coins))))
        (t (append
            (mapcar
             #'(lambda (x) (incf (car x)) x)
             (make-all-change (- amount (car coins)) coins))
            (mapcar #'(lambda (x) (cons 0 x))
                    (make-all-change amount (cdr coins))))))) 

;;version 2.0
(defun make-best-change (amount &optional (coins '(25 10 5 1)))
  (let ((change-lst (make-hash-table)))
    (do ((money 1 (1+ money)))
        ((> money amount) (gethash amount change-lst)) ;exit
      (dolist (coin-value coins)
        (do (buff nil (cons 
                       (judge-choice change-lst coin-value money) buff))
          (cond ((= coin-value money) (setf (gethash money change-lst) money))
              ((< coin-value money) (if (sum-list (gethash money change-lst))))))))))

(defun sum-larger (change-hash coin amount)
  (let ((old-change (gethash amount change-hash))
        (new-change (1+ (gethash (- amount coin) change-hash)))
        
  (apply #'+ nums))



       
                                       
(defun get-result (total result best-opt)
  (cond ((null best-opt) (cons result total))
        ((< total (cdr best-opt)) (cons result total))
        ((and (= total (cdr best-opt))
               (< (reduce #'+ result) (reduce #'+ (car best-opt))))
         (cons result total))
        (t best-opt)))    



;;Make-change perfect version
(defun make-change (amount &optional (coin-list '(25 10 5 1)))
  (let ((coin-lst nil))
    (values-list (mapcar #'(lambda (x) 
                             (multiple-value-setq (coin-lst amount) 
                                 (floor amount x))) coin-list))))



