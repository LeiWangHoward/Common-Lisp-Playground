;;Way too long!
(defmacro list-of (&rest args)
 ;functions to deal with generator forms
 (defun generator? (x) (and (listp x)(eq (cadr x) :in)))
 (defun var-generator (x) (car x))
 (defun list-generator (x) (caddr x))
 ;following function generates appropriate code given
 ;the expression and list of generators-filters
 (defun generate-code (exp gfs)
   (let ((code (gensym)))
     (defun recurse (gfs)
       (cond ((null gfs) `(push ,exp ,code))
             ((generator? (car gfs))
              `(dolist (,(var-generator (car gfs))
                         ,(list-generator (car gfs)))
                 ,(recurse (cdr gfs))))
             (t `(if ,(car gfs) ,(recurse (cdr gfs))))))
     `(let ((,code nil))
        ,(recurse gfs) (nreverse ,code))))
 ;extracting exp and list of generators-filters; then calling
 ;generate-code for code generation.
 (let ((exp (car args))
       (generators-filters (cdr args)))
   (if (generator? (car args))
       (progn (setf exp (var-generator (car args)))
              (setf generators-filters args)))
   (generate-code exp generators-filters))) 