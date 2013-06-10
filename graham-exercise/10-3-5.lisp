;;(pprint (macroexpand-1 '(nth-expr 3 (+ 1 2) (/ 1 0) (- 1 3))))
;;version 1.0 from yu liu
(defmacro nth-expr (n &body body)
  (let ((gn (gensym)))
    `(let ((,gn (1- ,n)))
       (case ,gn
         ,@(let ((i -1))
             (mapcar #'(lambda(x) `(,(incf i) ,x)) body))))))

;;version 1.1 use eval, cannot properly eval princ 

(defmacro nth-expr (n &body body) 
  `(eval (nth (1- ,n) ',body))) 

;;version 1.2 from other's
(defmacro nth-expr (n &rest expressions)
    `(case ,n
        ,@(loop for e in expressions
                for n from 1
                collect
                `((,n) ,e))))



;;n-of v 1.0 (use with-gensyms)
(defmacro n-of (n expr)
  (with-gensyms (gn gi gacc)
    `(do ((,gn ,n) (,gi 0 (1+ ,gi)) (,gacc nil (cons ,expr ,gacc)))
         ((= ,gi ,gn) (nreverse ,gacc)))))

(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
                     `(,s (gensym)))
                 syms)
     ,@body))

;; altertive (not good)
(defmacro n-of (n expr)
  (let((grec (gensym)))
    `(labels ((,grec (i j acc)
                (if (= i j)
                    (nreverse acc)
                  (,grec (1+ i) j (cons ,expr acc)))))
       (,grec 0 ,n nil))))

;;n-of final version
(defmacro n-of (n expr)
  (let ((gn (gensym))
        (gi (gensym))
        (glst (gensym)))
    `(do ((,gn ,n) (,gi 0 (1+ ,gi)) (,glst nil (cons ,expr ,glst)))
         ((= ,gi ,gn) (nreverse ,glst)))))
