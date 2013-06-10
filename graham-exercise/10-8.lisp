;;preserve
(defmacro preserve (parms &body body)
  `((lambda ,parms ,@body) ,@parms))


;;v 1.0 cannot pas final test
(defmacro doublef (n)
  (let ((gx (gensym)))
    `(let ((,gx ,n))
       (setf ,n (* 2 ,gx)))))

(defmacro doublef (n)
  `(setf ,n (* 2 ,n)))

;;v1.1
(defmacro doublef (n)
  (let ((gx (gensym)))
    `(let ((,gx 
            (if (listp ',n) (lambda ',n ,n) ,n)))
       (setf ,n (* 2 ,gx)))))

;;v1.2 (not working)
(defmacro doublef (n)
  (let ((gx (gensym)))
    `(let ((,gx 
            (lambda ,n (* 2 ,n))))
       ,gx)))

;;
#|
;;v 1.1 not work
(defmacro doublef (n)
  (let ((parms (gensym))
        (body (gensym)))
  `(let((,parms (cdr ,n))
        (,body (car ,n)))
     (lambda ,parms ,@body) ,@parms)))

;;v 1.2 not work
(defmacro doublef (n)
  (let ((gx (gensym))
        (gbody (gensym)))
    `(let* ((,gbody ',n)
            (,gx (* 2 ,n)))
       (setf ,n ,gbody)
       (setf ,n ,gx))))
|#