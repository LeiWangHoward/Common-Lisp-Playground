;version 1.0 works but with bug and "too long"
(defun pattern-words (word trie)
  (let ((lst nil)
         (letter (elt word 0))
         (rest (subseq word 1)))
    (cond ((null trie) nil)
          ((equal rest "")
           (if (eql #\? letter)
               (loop for (key . item) in (trie-branch trie)
                     do (push (trie-indicator (cdr (assoc key (trie-branch trie)))) lst))
             (push (trie-indicator (cdr (assoc letter (trie-branch trie)))) lst)))
          (t (if (eql #\? letter)
                 (loop for (key . item) in (trie-branch trie)
                       do (setf lst (nconc (pattern-words rest (cdr (assoc key (trie-branch trie)))) lst)))
               (setf lst (pattern-words rest (cdr (assoc letter (trie-branch trie))))))))
    (remove nil lst)))

;version 1.1 everything works
(defun pattern-words (word trie)
  (let ((lst nil)
         (letter (elt word 0))
         (rest (subseq word 1)))
    (cond ((null trie) nil)
          ((equal rest "")
           (if (eql #\? letter)
               (loop for (key . item) in (trie-branch trie)
                     do (push (trie-word (cdr (assoc key (trie-branch trie)))) lst))
             (push (trie-word (cdr (assoc letter (trie-branch trie)))) lst)))
          (t (if (eql #\? letter)
                 (loop for (key . item) in (trie-branch trie)
                       do (setf lst (nconc (pattern-words rest (cdr (assoc key (trie-branch trie)))) lst)))
               (setf lst (pattern-words rest (cdr (assoc letter (trie-branch trie))))))))
    (remove nil lst)))

(defun trie-word (trie)
  (if (not (null trie))(trie-indicator trie) nil))

;version 1.2 great version
(defun pattern-words (word trie)
  (let ((lst nil)
         (alpha (elt word 0))
         (rest (subseq word 1)))
    (cond ((null trie) nil)
          ((equal rest "")
           (if (eql #\? alpha)
               (loop for (key . item) in (trie-branch trie)
                     do (push (trie-word (subtrie trie key)) lst))
             (push (trie-word (subtrie trie alpha)) lst)))
          (t (if (eql #\? alpha)
                 (loop for (key . item) in (trie-branch trie)
                       do (setf lst (nconc (pattern-words rest (subtrie trie key)) lst)))
               (setf lst (pattern-words rest (subtrie trie alpha))))))
    (remove nil lst)))

(defun trie-word (trie)
  (if (not (null trie))
      (trie-indicator trie) 
    nil))

#|
;version 2.0 works on exact match
(defun pattern-words(word trie)
  (let ((lst-subtrie nil);;lst-words
        (lst-char (coerce word 'list)))
        (trie-word (subtrie trie (values-list lst-char)))
        ))

;;version 3.0 works on everything?not
(defun pattern-words(word trie)
  (let ((lst-tries nil);;lst-words
        (sub nil)
        (lst-char (coerce word 'list)))
    (dolist (item lst-char)
      (if (eql #\? item)
          (loop for (key . item) in (trie-branch trie)
                do (push (subtrie item) sub))))sub))|#



        
