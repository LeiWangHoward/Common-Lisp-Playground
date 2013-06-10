(defun make-queue (&rest body) 
  (if (null body)
      (cons nil nil)
    (cons body (last body))))

(defun empty-queue-p (q)
  (if (null (car q))
      t
    nil))

(defun enqueue (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
    (setf (cdr (cdr q)) (list obj)
          (cdr q) (cdr (cdr q))))
  (car q))

(defun dequeue (q)
  (pop (car q)))

(defun copy-queue (q)
  (let ((q1 (make-queue)))
    (setf (car q1) (copy-list (car q))
          (cdr q1) (last (car q1)))
    q1))

(defun enqueue-front (obj q)
  (if (null (car q))
      (setf (cdr q) (setf (car q) (list obj)))
    (push obj (car q))))

(defun requeue-front (obj q)
  (let ((q1 (car q)))
    (setf (car q) (if (member obj q1)
                      (cons obj (remove obj q1 :count 1))
                    q1)
          (cdr q) (last (car q)))))

 #|
(defun remove-first (elt seq)
  (let ((remove-first t))
    (remove-if (lambda (e) (when (and remove-first (equal elt e))
                             (setq remove-first nil)
                             t))
               seq)))|#
