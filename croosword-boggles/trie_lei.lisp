;;;;trie 

;;; Trie class
(defclass trie ()
  ((alpha-value :accessor trie-alpha-value
                :initarg  alpha-value)
   (branches    :accessor trie-branches
                :initarg  branches))
  (:default-initargs
   :alpha-value nil
   :branches (make-hash-table))
  (:documentation "Trie node Representation"))

(export 'trie)
 
(defmethod add-word ((string trie))
  trie)

(defun navigate-trie (trie key-list &key create-new-branches) 
  (with-slots (value branches) trie
    (if (null key-list) (values trie value) ; No more keys to process--return the current node. 
      (destructuring-bind (next-key &rest remaining-keys) key-list
        (progn
          (unless (gethash next-key branches) ;; Not found. Create a new branch or fail. 
            (if create-new-branches
                (setf (gethash next-key branches) (make-instance 'trie))
              (return-from navigate-trie (values nil value)))) ;; Descend down the tree. 
          (navigate-trie (gethash next-key branches)
                         remaining-keys
                         :create-new-branches create-new-branches))))))

(defgeneric get-trie (key-list trie)
  (:method ((key-list list) (trie trie))
   (awhen (navigate-trie trie key-list)
          (trie-value it))))
 
(export 'get-trie)

(defgeneric lookup-prefix-in-trie (key-list trie)
  (:documentation "Looks up the longest defined prefix of the supplied key in the trie.")
  (:method ((key-list list) (trie trie))
   (multiple-value-bind (_ value)
       (navigate-trie trie key-list)
     (declare (ignore _)) value)))

(export 'lookup-prefix-in-trie) 



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

(test "trie"
      '((nil 1 2 3 nil nil nil 4 nil)
        (nil nil 1 2 2))
      (let ((trie (make-instance 'trie)))
        (bulk-insert-trie '(((a) 1)
                            ((a a) 2)
                            ((a b) 3)
                            ((b a) 4))
                          trie)
        (list (mapcar #'(lambda (key-list) (get-trie key-list trie))
                      '(nil (a) (a a) (a b) (a c) (a c d) (b) (b a) (b a e)))
              (mapcar #'(lambda (key-list) (lookup-prefix-in-trie key-list trie))
                      '(nil (z) (a) (a a) (a a a))))))
;;;(add-word string trie): adds a word, in lower case, to a trie node and returns the trie. The trie should be destructively modified.
;;;(subtrie trie char1 char2 ...): given a trie and zero or more characters, returns the root of the subtrie for that sequence of characters, if any, or nil.
;;;(trie-word trie): returns the word at a trie node if any, or nil.
;;;(trie-count trie): returns the number of words stored at or under this trie node.
;;;(mapc-trie fn trie): given a function and a trie, calls (fn char subtrie) with the character and subtrie for every branch immediately under trie. Called for effect. Returns trie.
;;;(read-words file trie): Reads a file of words into trie. Returns trie. The file should contain one word per line.

