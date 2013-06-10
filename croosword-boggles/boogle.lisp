;;; boggle.lisp
;;; Copyright 2001, Jason Cornez
;;; Everyone is freely given the right to do anything with this code.
;;; This is not guaranteed to do anything.
;;; If somehow you find it useful and copy it, please give credit.
;;;
;;; Description:
;;; Boggle is a game whose physical incarnation consists of 16 dice
;;; containg letters on each side.  "Qu" is shown together on a single
;;; side.  The boggle box is shaken so that the dice become randomly
;;; arranged in a 4 x 4 grid.  The object is to find as many words as
;;; possible starting with any letter and choosing consecutive letters
;;; such that they are adjacent in any direction to the preceeding
;;; letter.  In addition, each die may only be used once per word.
;;; There is a three minute timer.  Scoring is one point for 3 or 4
;;; letter words and an additional point for each additional letter in
;;; longer words.  "Qu" is counted as 2 letters, even though it is on
;;; a single die.
;;;
;;; Status:
;;; So far the game has no real user interface, no timer, and no way
;;; for more than one player to interact (since there is no
;;; interface).  What does exist are the boggle dice and the ability
;;; to shake a box, to pretty-print the box, and then to validate and
;;; score words against the box.  Ultimately, the above listed missing
;;; components should be added and perhaps also a check for words in
;;; some dictionary.  This was written for fun as an exercise, so none
;;; of those are high priorities.


(in-package "user")

;; The 16 boggle dice
(defconstant +boggle-dice+ (make-array '(16 6) :initial-contents
                                       '((#\T #\Y #\E #\L #\R #\T)
                                         (#\N #\H #\N #\L #\R #\Z)
                                         (#\E #\G #\N #\W #\H #\E)
                                         (#\U #\N #\H #\I #\M #\Q)
                                         (#\D #\R #\Y #\V #\L #\E)
                                         (#\K #\S #\F #\F #\A #\P)
                                         (#\J #\B #\O #\O #\B #\A)
                                         (#\X #\I #\D #\R #\E #\L)
                                         (#\S #\T #\I #\T #\Y #\D)
                                         (#\O #\T #\W #\O #\T #\A)
                                         (#\O #\S #\C #\A #\H #\P)
                                         (#\E #\E #\G #\N #\A #\A)
                                         (#\T #\M #\I #\O #\U #\C)
                                         (#\S #\O #\I #\S #\T #\E)
                                         (#\W #\R #\E #\T #\H #\V)
                                         (#\U #\I #\E #\N #\E #\S))))
    
(defun shake-boggle-box ()
  "Return a 4x4 array of characters representing a shaken boggle box."  
  (let ((dice (make-array 16))
        (box (make-array '(4 4))))

    ;; fill dice with 0 to 15
    (loop for i from 0 to 15
        do (setf (aref dice i) i))

    ;; shuffle the dice, so we can't choose same one twice
    (loop for i from 0 upto 15
          for rand = (random 16) do
          (psetf (aref dice i) (aref dice rand)
                 (aref dice rand) (aref dice i)))
    
    ;; fill the box with the dice, choosing a random side for each
    (loop for i from 0 to 15 do
          (setf (row-major-aref box i)
            (aref +boggle-dice+ (aref dice i) (random 6))))

    box))

(defun pprint-boggle-box (box)
  "Pretty print a boggle box as 4 rows or 4 evenly spaced characters."
  (loop for i from 0 to 3 do
        (princ "  ")
        (loop for j from 0 to 3 do
              (princ (aref box i j))
              (if (char-equal (aref box i j) #\Q)
                  (princ "u ")          ; after "Q" print "u"
                (princ "  ")))
        (fresh-line)))

(defun score-word (word box)
  "Calculate the score of the word for the given boggle box."
  (cond ((< (length word) 3)
         (values 0 :Too-Small))
        ((valid-word-p word box)
         (values (max 1 (- (length word) 3)) :OK))
        (t
         (values 0 :Not-Found))))

(defun valid-word-p (word box)
  "Return true if the word exists in the boggle box."
  ;; for each die in the box, see if the word can start from there
  ;; exit immediately if the word is found
  (loop for i from 0 to 3 do
        (loop for j from 0 to 3
            when (valid-word-from-p word box i j '()) do
              (return-from valid-word-p t))))

(defun valid-word-from-p (word box x y already-used)
  "Recursively try to find word in box starting at location x, y.  Keep
track to make sure each location is only used once."
  (let ((first (elt word 0))            ; first letter of word
        (rest (subseq word 1))          ; rest of word
        (index (+ x (* 4 y)))           ; unique index of current die
        (neighbors '()))                ; list of neighbors of current
die

    ;; if the die has been used or its letter doesn't match, return nil
    (when (or (member index already-used)
              (char-not-equal first (aref box x y)))
      (return-from valid-word-from-p nil))
    
    ;; if on a "Q" and it is followed by "u" in word, skip searching
    ;; for the "u" since it is included on the "Q" die.
    (when (and (char-equal #\Q first)
               (char-equal #\u (elt rest 0)))
      (setq rest (subseq rest 1)))

    ;; if there is no more left to the word, we found it! return t
    (when (= (length rest) 0)
      (return-from valid-word-from-p t))

    ;; compute list of neighbors.  wrapping around edges not allowed.
    (loop for i from (max 0 (1- x)) to (min 3 (1+ x)) do
          (loop for j from (max 0 (1- y)) to (min 3 (1+ y)) do
                (unless (and (= i x) (= j y))
                  (push (list i j) neighbors))))

    ;; for each neighbor, test the rest of the word remembering where
    ;; we've been.  exit immediately upon success
    (loop for (i j) in neighbors
        when (valid-word-from-p rest box i j (cons index already-used))
        return t)))

(defun score-words (word-list box)
  "Sum the scores of a list of words for a boggle box."
  (loop for word in word-list
      sum (score-word word box)))