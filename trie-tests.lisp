;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Word Trie Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage trie-tests
  (:use :common-lisp trie lisp-unit))

;;; 11-02-2010 [CKR] changed MAPC-TRIE test to UNORDERED-EQUAL
;;;     added test to SUBTRIE, changed call to READ-WORDS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package trie-tests)

(defvar *trie* nil)

(defparameter *word-file* 
  (merge-pathnames "crosswd.txt"
                   (or *compile-file-truename* *load-truename*)))

(defun new-trie (&rest words)
  (add-words words (make-trie)))

(defun add-words (words trie)
  (dolist (word words trie)
    (add-word word trie)))

(define-test subtrie
    (assert-false (subtrie (make-trie) #\a))
  (let ((trie (new-trie "abc" "def")))
    (assert-true (subtrie trie #\a))
    (assert-true (subtrie trie #\d))
    (assert-false (subtrie trie #\b))
    (assert-true (subtrie trie #\a #\b))
    (assert-eq trie (subtrie trie))
    ))

(define-test trie-word
    (assert-false (trie-word (make-trie)))
  (let ((trie (new-trie "abc" "def")))
    (assert-false (trie-word trie))
    (assert-false (trie-word (subtrie trie #\a)))
    (assert-false (trie-word (subtrie trie #\a #\b)))
    (assert-equal "abc" (trie-word (subtrie trie #\a #\b #\c)))
    ))

(define-test trie-count
    (assert-equal 0 (trie-count (make-trie)))
  (assert-equal 1 (trie-count (new-trie "abc")))
  (let ((trie (new-trie "ab" "abc" "abd" "b")))
    (assert-equal 4 (trie-count trie))
    (assert-equal 3 (trie-count (subtrie trie #\a)))
    (assert-equal 3 (trie-count (subtrie trie #\a #\b)))
    (assert-equal 1 (trie-count (subtrie trie #\a #\b #\c)))
    (assert-equal 1 (trie-count (subtrie trie #\a #\b #\d)))
    ))


(define-test read-words
    (setq *trie*
          (read-words *word-file* (make-trie)))
  (assert-equal 113809 (trie-count *trie*))
  )

(defun collect-chars (trie)
  (let ((l nil))
    (mapc-trie #'(lambda (ch subtrie) (push ch l)) trie)
    (nreverse l)))

(define-test mapc-trie
    (let ((trie (new-trie "ab" "abc" "abd" "b")))
      (assert-equality 'unordered-equal '(#\a #\b) (collect-chars trie))
      (assert-equality 'unordered-equal '(#\c #\d) (collect-chars (subtrie trie #\a #\b)))
      (assert-equal nil (collect-chars (subtrie trie #\b)))
      ))

;;; This tests the Crossword Helper
(define-test pattern-words
    (let ((trie (new-trie "abc" "abd" "aec" "bdc")))
      (assert-equal '("aec") (pattern-words "aec" trie))
      (assert-equal '("abc" "abd") (pattern-words "ab?" trie))
      (assert-equal '("abc" "aec") (pattern-words "a?c" trie))
      (assert-equal '("abc" "aec" "bdc") (pattern-words "??c" trie))
      (assert-equal '("abc" "abd" "aec" "bdc") (pattern-words "???" trie))
      )
  (when (null *trie*)
    (setq *trie* (read-words *word-file* (make-trie))))
  (assert-equal '("capelan" "capelin" "gamelan" "javelin" "lanolin" "maudlin"
                  "papulan" "ravelin")
                (pattern-words "?a??l?n" *trie*))
  )
                

      

      