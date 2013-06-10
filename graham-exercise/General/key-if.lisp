;;v 1.0: not working at all
(defmacro key-if (test &rest exps)
  (let ((e (gensym)) (flag (gensym)) (then nil) (else nil))
    (dolist (e exps)
      (cond
       ((eql e :then) (setf flag t))
       ((eql e :else) (setf flag nil))
       (flag (push e then))
       (t (push e else)))) 
    `(cond (,test ,@(reverse then)) 
           (t ,@(reverse else)))))

;;v 1.2 works but a little too long
(defmacro key-if (test &rest exps)
  (let ((e (gensym)) (flag (gensym)) 
        (then (if (member :then exps) nil '(nil)))
        (else (if (member :else exps) nil '(nil))))
    (dolist (e exps)
      (cond
       ((eql e :then) (setf flag t))
       ((eql e :else) (setf flag nil))
       (flag (push e then))
       (t (push e else))))
    `(cond (,test ,@(reverse then)) 
           (t ,@(reverse else)))))

;;v 1.3
(defmacro key-if (test &rest exps)
  (let* ((then (gensym))
         (else (gensym)) 
         (then (cdr (member :then exps)))
         (then (if (null then) '(nil)
                 (ldiff then (member :else then))))
         (else (cdr (member :else exps)))
         (else (if (null else) '(nil)
                 (ldiff else (member :then else)))))
    `(cond (,test ,@then) 
           (t ,@else))))





#|
(defmacro key-if (test &rest exps)
  (let ((then (gensym)) (else (gensym)))
    (setf then (cdr (ldiff :else exps))
          else (cdr (ldiff :then exps)))
    `(cond (,test ,@then) 
           (t ,@else))))
|#