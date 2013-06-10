(defpackage "TRIVIAL-TRIE"
    (:use "CL" "SB-EXT"))

(in-package "TRIVIAL-TRIE")

(defconstant +trie-fan-out+ 26)

(declaim (inline char-index))
(defun char-index (char)
  (declare (type base-char char)
           (optimize speed))
  #+ (or) ; this is at the very least safe on erroneous input
  (let ((idx (logand (- (char-code char) (char-code #\a))
                     #x255)))
    (declare (type fixnum idx))
    (truly-the
     (mod #. +trie-fan-out+)
     (if (<= idx (1- +trie-fan-out+))
         idx
         (1- +trie-fan-out+))))
  #+ (and)
  (truly-the
   (mod #. +trie-fan-out+)
   (- (char-code char) (char-code #\a))))

;; less indirection
#+ (or)
(progn
  (deftype trie-node ()
    `(simple-array t (,(1+ +trie-fan-out+))))

  (declaim (inline trie-node-fullp (setf trie-node-fullp)))
  (defun trie-node-fullp (node)
    (declare (type trie-node node))
    (aref node +trie-fan-out+))
  (defun (setf trie-node-fullp) (value node)
    (declare (type trie-node node))
    (setf (aref node +trie-fan-out+) value))
  
  (defvar *empty-leaf-node*
    (let ((node (make-array (1+ +trie-fan-out+))))
      (fill node node :end +trie-fan-out+)
      (setf (aref node +trie-fan-out+) nil)
      node))
  (define-symbol-macro %empty-leaf%
      (load-time-value (the trie-node *empty-leaf-node*) t))

  (defvar *full-leaf-node*
    (let ((node (make-array (1+ +trie-fan-out+))))
      (fill node %empty-leaf% :end +trie-fan-out+)
      (setf (aref node +trie-fan-out+) t)
      node))
  (define-symbol-macro %full-leaf%
      (load-time-value (the trie-node *full-leaf-node*) t))

  (defun make-inner-node (fullp)
    (let ((node (make-array (1+ +trie-fan-out+) :initial-element %empty-leaf%)))
      (setf (aref node +trie-fan-out+) fullp)
      node))

  (defstruct trie
    (root %empty-leaf% :type trie-node))

  (defun trie-insert (string trie &key (start 0) (end (length string)))
    (declare (simple-base-string string)
             (type (and unsigned-byte fixnum) start end)
             (trie trie))
    (let ((length end))
      (assert (<= length (length string)))
      (assert (<= start end))
      (labels ((inner (index trie)
                 (declare (type (and fixnum unsigned-byte) index)
                          (type trie-node trie)
                          (optimize speed (safety 0)))
                 (cond ((= index length)
                        (if (eq trie %empty-leaf%)
                            %full-leaf%
                            (prog1 trie
                              (setf (trie-node-fullp trie) t))))
                       (t
                        (when (or (eq trie %empty-leaf%)
                                  (eq trie %full-leaf%))
                          (setf trie (make-inner-node
                                      (not (eq trie %empty-leaf%)))))
                        (let ((char (char-index (aref string index))))
                          (setf (aref trie char)
                                (inner (1+ index) (aref trie char)))
                          trie)))))
        (setf (trie-root trie)
              (inner start (trie-root trie)))
        string)))

  (defun trie-lookup (string trie &key (start 0) (end (length string)))
    (declare (simple-base-string string)
             (type (and unsigned-byte fixnum) start end)
             (trie trie))
    (let ((length end))
      (assert (<= length (length string)))
      (assert (<= start end))
      (labels ((inner (index trie)
                 (declare (type (and fixnum unsigned-byte) index)
                          (type trie-node trie)
                          (optimize speed (safety 0)))
                 (cond ((= index length)
                        (trie-node-fullp trie))
                       ((not (or (eq trie %empty-leaf%)
                                 (eq trie %full-leaf%)))
                        (let ((char (char-index (aref string index))))
                          (inner (1+ index) (aref trie char)))))))
        (inner start (trie-root trie))))))

;; more idiomatic
#+ (and)
(progn
  (declaim (inline trie-node-fullp))
  (defstruct (trie-node
               (:constructor %make-trie-node (fullp)))
    ;; T iff a word "finishes" here
    (fullp nil :type boolean))

  (declaim (inline (setf trie-node-fullp)))
  (defun (setf trie-node-fullp) (value node)
    (declare (ignore value node))
    (error "FULLP only mutable for interior nodes"))

  (defvar *empty-leaf-node* (%make-trie-node nil))
  (define-symbol-macro %empty-leaf%
      (load-time-value (the trie-node *empty-leaf-node*)
                       t))

  (defvar *full-leaf-node* (%make-trie-node t))
  (define-symbol-macro %full-leaf%
      (load-time-value (the trie-node *full-leaf-node*)
                       t))

  (declaim (inline empty-leaf-p))
  (defun empty-leaf-p (node)
    (eq node %empty-leaf%))

  (deftype children-vector ()
    `(simple-array trie-node (,+trie-fan-out+)))

  (declaim (inline make-empty-children))
  (defun make-empty-children ()
    (make-array +trie-fan-out+
                :initial-element %empty-leaf%
                :element-type 'trie-node))

  (declaim (inline make-interior-node))
  (defstruct (trie-interior-node (:include trie-node)
               (:constructor make-interior-node (fullp)))
    (children (make-empty-children) :type children-vector))

  (defstruct trie
    (root %empty-leaf% :type trie-node))

  (declaim (freeze-type trie-node trie-interior-node))

  (defun trie-insert (string trie &key (start 0) (end (length string)))
    (declare (simple-base-string string)
             (type (and unsigned-byte fixnum) start end)
             (trie trie))
    (let ((length end))
      (assert (<= length (length string)))
      (assert (<= start end))
      (labels
          ((inner (index trie)
             (declare (type (and fixnum unsigned-byte) index)
                      (type trie-node trie)
                      (optimize speed (safety 1)))
             (cond ((= index length)
                    (if (trie-interior-node-p trie)
                        (prog1 trie
                          (setf (trie-interior-node-fullp trie) t))
                        %full-leaf%))
                   (t
                    (unless (trie-interior-node-p trie)
                      (setf trie (make-interior-node
                                  (not (empty-leaf-p trie)))))
                    (let ((char (char-index (aref string index)))
                          (children (trie-interior-node-children trie)))
                      (setf (aref children char)
                            (inner (1+ index)
                                   (aref children char))))
                    trie))))
        (setf (trie-root trie)
              (inner 0 (trie-root trie)))
        (values))))

  (defun trie-lookup (string trie &key (start 0) (end (length string)))
    (declare (simple-base-string string)
             (type (and unsigned-byte fixnum) start end)
             (trie trie))
    (let ((length end))
      (assert (<= length (length string)))
      (assert (<= start end))
      (labels
          ((inner (index trie)
             (declare (type (and fixnum unsigned-byte) index)
                      (type trie-node trie)
                      (optimize speed (safety 0)))
             (cond ((= index length)
                    (trie-node-fullp trie))
                   ((trie-interior-node-p trie)
                    (let ((char (char-index (aref string index))))
                      (inner (1+ index)
                             (aref (trie-interior-node-children trie)
                                   char)))))))
        (inner 0 (trie-root trie))))))

(defun read-words (file)
  (let ((words (make-array 1024 :fill-pointer 0 :adjustable t)))
    (with-open-file (s file)
      (loop for line = (read-line s nil nil)
            while line
            do (vector-push-extend (coerce (string-downcase line)
                                           'simple-base-string)
                      words)))
    (make-array (length words) :initial-contents words)))

(defun shuffle-vector (vector)
  (declare (type (simple-array t 1) vector))
  (loop with state = *random-state*
        for i from (1- (length vector)) downto 0
        for other = (random (1+ i) state)
        do (rotatef (aref vector i) (aref vector other))
        finally (return vector)))

(defun insert-words (words)
  (declare (type (simple-array t 1) words)
           (optimize speed))
  (let ((trie (make-trie)))
    (sb-vm::without-gcing
      ;; this doesn't cons any garbage up.
      ;; if we run out of space, we're fscked anyway
      (map nil (lambda (word)
                 (trie-insert word trie))
           words))
    trie))

(defun lookup-words (trie words)
  (declare (type (simple-array t 1) words)
           (optimize speed))
  (map nil (lambda (word)
             (trie-lookup word trie))
       words))

(defun lookup-mixed (trie words1 words2)
  (declare (type (simple-array t 1) words1 words2)
           (optimize speed))
  (map nil (lambda (word1 word2)
             (trie-lookup word1 trie)
             (trie-lookup word2 trie))
       words1 words2))

(defun test-bench (file)
  (let* ((words (read-words file))
         (in    (subseq words 0 (floor (length words) 2)))
         (out   (subseq words (ceiling (length words) 2)))
         (trie  (time
                 (progn
                  (gc :full t)
                  (insert-words in)))))
    (time (lookup-words trie out))
    (time (lookup-words trie in))
    (time (lookup-mixed trie in out))))