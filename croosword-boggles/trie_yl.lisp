(defpackage #:trie
(:use #:common-lisp)
(:export #:trie #:make-trie #:subtrie #:add-word #:trie-word #:trie-count #:mapc-trie #:read-words))
(in-package #:trie)

(defun add-word(word trie)
  (cond((equal(subseq word 1) "")
        (if(gethash(elt word 0)(trie-b trie))
            (setf(trie-wd(gethash(elt word 0)(trie-b trie)))#\.)
          (setf(gethash(elt word 0)(trie-b trie))(make-trie)
            (trie-wd(gethash(elt word 0)(trie-b trie)))#\.)))
        (t(unless(gethash (elt word 0)(trie-b trie))
            (setf(gethash(elt word 0)(trie-b trie))
              (make-trie)))
           (add-word(subseq word 1)(gethash(elt word 0)(trie-b trie))))))
            
          
(setf trie (make-hash-table))                   
(defstruct trie
  (b(make-hash-table)))

(defun subtrie(trie &rest args)
  (s2 trie args))
  
(defun s2(trie lst)
  (cond((null lst)trie)      
        ((and(null(cdr lst))
              (gethash(car lst)(trie-b trie)))
         (gethash(car lst)(trie-b trie)))
        ((gethash(car lst)(trie-b trie))
         (s2(gethash(car lst)(trie-b trie))(cdr lst)))
        (t nil)))

(defun trie-word(trie)
  (trie-wd trie))

(defun trie-count(trie)
  (let((total 0))
    (when(trie-wd trie)
      (incf total))
    (loop for key being the hash-keys of (trie-b trie) using (hash-value value)
          do(incf total (trie-count value)))
    total))

(defun mapc-trie(fn trie)
  (loop for key being the hash-keys of (trie-b trie) using (hash-value value)
          do(funcall fn key value)))
  
(defun read-words(file trie)
  (with-open-file(str file :direction :input)
    (do((line(read-line str nil 'eof)
              (read-line str nil 'eof)))
        ((eql line 'eof)trie)
      (add-word line trie))))



