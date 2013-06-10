(defpackage #:trie
  (:use #:common-lisp)
  (:export #:trie #:make-trie #:subtrie #:add-word #:trie-word #:trie-count #:mapc-trie #:read-words))
(in-package #:trie)
;; Use make-trie function for test CLOS version
(defun make-trie ()
  (make-instance 'trie))

(defclass trie ()
  ((word-count :accessor wcount
               :initform 0
               :initarg :word-count)
   (branch :accessor trie-branch
           :initform 'nil
           :initarg :branch)
   (indicator :accessor trie-indicator
              :initform 'nil
              :initarg :indicator)))

(defun add-word (string trie) ;;iterative method for looping through string
  (let ((lst-char (coerce string 'list))
        (tri trie))
    (dolist (item lst-char)
      (cond
       ((assoc item (trie-branch tri))
        (incf (wcount tri))
        (setf tri (cdr (assoc item (trie-branch tri)))))
       (t
        (incf (wcount tri))
        (setf (trie-branch tri) (acons item (make-instance 'trie) (trie-branch tri)))
        (setf tri (cdr (assoc item (trie-branch tri)))))))
    (setf (trie-indicator tri) string)))

(defun subtrie (trie &rest args)
  (labels ((sub (trie chars)
             (cond ((null chars) trie)      
                   ((assoc (car chars) (trie-branch trie))
                    (if (null (cdr chars))
                        (cdr (assoc (car chars) (trie-branch trie)))
                      (sub (cdr (assoc (car chars) (trie-branch trie))) (cdr chars))))
                   (t nil))))
    (sub trie args)))

(defun trie-word (trie)
  (trie-indicator trie))

(defun trie-count (trie)
  (wcount trie))

(defun mapc-trie (fn trie)
  (loop for (key . item) in (trie-branch trie)
           do (funcall fn key item)))

(defun read-words (file trie)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil 'eof)
               (read-line str nil 'eof)))
        ((eql line 'eof) trie)
      (add-word line trie))))
