;;v 1.0 not working so well
(defstruct (tconc (:constructor create-tconc))
  "a tail concatenate list for easy append"
  (head nil) (tail nil))
(defun make-tconc (&optional l)
  (let ( (tc (create-tconc)))
    (apply #'tconc tc l)
    tc))

(defun tconc (tconc-structure &rest items)
  (unless (null items)
    (if (null (tconc-head tconc-structure))
        (setf (tconc-head tconc-structure) items)
      (setf (cdr (tconc-tail tconc-structure)) items))
    (setf (tconc-tail tconc-structure) (last items)))
  (tconc-head tconc-structure))

(defun tconc-list (tconc-structure &optional l)
  (apply #'tconc tconc-structure l)) 

;;v 1.1 My version
(defstruct (tconc (:constructor create-tconc))
  (head nil) (tail nil))

(defun make-tconc (&optional l)
  (let ((tc (create-tconc)))
    (setf (tconc-head tc) l
          (tconc-tail tc) (last l))
    tc))
    
    ;;(apply #'tconc tc l)
    ;;  tc))

(defun tconc (tconc-structure &rest items)
  (unless (null items)
    (if (null (tconc-head tconc-structure))
        (setf (tconc-head tconc-structure) items)
      (setf (cdr (tconc-tail tconc-structure)) items))
    (setf (tconc-tail tconc-structure) (last items)))
  (tconc-head tconc-structure))

(defun tconc-list (tconc-structure &optional l)
  (apply #'tconc tconc-structure l)) 
