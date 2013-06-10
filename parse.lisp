;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Simple MOP-based Parser
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; 02/26/08 Updated to CL2 packages and new mops code [CKR]
;;; 11/10/95 Created file [CKR]


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require "mops")

(defpackage #:parse 
  (:use #:common-lisp #:mops)
  (:export #:defphrase #:parse)
  )

(in-package #:parse)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals and structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Concept phrase pairs join a concept with a phrase that can parse
;;; to that concept.

(defstruct cp concept phrase)

;;; *CONCEPT-PHRASES* holds all the concept-phrase pairs.

(defvar *concept-phrases* nil "All concept-phrase pairs")

;;; A parse has a concept, role bindings for the concept, the input
;;; words that were used, and the tail of the input that was
;;; not used, if any.

(defstruct (parse (:print-function print-parse))
  concept bindings used input)

(defun print-parse (parse stream recur)
  (format stream "#<~S~{ ~S~} Used: ~S~@[ Unused: ~S~]>"
    (parse-concept parse) (parse-bindings parse)
    (parse-used parse) (parse-input parse)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; DEFPHRASE, CONCEPT-PHRASES, PHRASE-CONCEPTS, MAP-PHRASES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro defphrase (concept &rest phrase)
  (when (endp phrase)
     (error "~S not a legal phrase" phrase))
  `(add-phrase ',concept ',phrase))

(defun add-phrase (concept phrase)
  (when (endp phrase)
    (error "~S not a legal phrase" phrase))
  (pushnew (make-cp :concept concept :phrase phrase)
           *concept-phrases*
           :test #'equalp)
  phrase)

(defun phrase-concepts (x)
  (let ((phrase (if (listp x) x (list x))))
    (loop for cp in *concept-phrases*
          when (equal phrase (cp-phrase cp))
          collect (cp-concept cp))))

(defun concept-phrases (concept)
  (loop for cp in *concept-phrases*
        when (eql concept (cp-concept cp))
        collect (cp-phrase cp)))
 
(defun map-phrases (fn)
  (mapc #'(lambda (cp)
            (funcall fn (cp-concept cp) (cp-phrase cp)))
        *concept-phrases*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; PARSE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (PARSE input) => mop instance; best parses; all parses
;;;;  Returns a mop instance for the best parse, if unique,
;;;   plus a list of best parses, plus a list of all parses.
;;;
;;; Parses are ranked by amount of input matched (the more,
;;; the better), and specificity of concepts (the more
;;; specific, the better).
;;;
;;; Examples from parse-tests.lisp:
;;; (PARSE '(CATCH A BUTTERFLY)) -> catch an animal
;;; (PARSE '(CATCH A COLD)) -> catch a disease
;;; (PARSE '(CATCH A BUG)) -> ambiguous

(defun parse (input)
  (let* ((all-parses (match-cp-list (fetch-cp-list input) input))
         (best-parses (get-best-parses all-parses)))
    (values (if (or (null best-parses)
                    (not (null (cdr best-parses))))
                nil
              (add-mop-instances (car best-parses)))
            best-parses
            all-parses)))

(defun add-mop-instances (parse)
  (if (null parse) nil
    (let ((slots (parse-slots (parse-bindings parse))))
      (add-instance (parse-concept parse) slots))))

(defun parse-slots (bindings)
  (mapcan #'(lambda (binding)
              (list (car binding) (caadr binding)))
    bindings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FETCH-CP-LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (FETCH-CP-LIST input) => list of concept phrase pairs
;;;    Returns all concept phrases that might parse input, which
;;;    is a list of words.
;;; 
;;; This function should be cheap (low CONSing), return all phrases
;;; that might parse some part of input, but discriminating enough
;;; to avoid sending obviously unfit phrases to detailed matching.
;;;
;;; The implementation below returns every phrase that:
;;;
;;;   - is no longer than the input
;;;   - has no words not in the input
;;;   - has words in the same relative order as the input
;;;
;;; Note that
;;;  - the input can have words not in the phrase
;;;  - a phrase with all keywords only rejects inputs shorter
;;;    than it.
;;;
;;; The only CONSing is the building of the list of concept phrase
;;; pairs in FETCH-CP-LIST.

(defun fetch-cp-list (input)
  (loop for cp in *concept-phrases*
        when (phrase-possible-p (cp-phrase cp) input)
        collect cp))

(defun phrase-possible-p (phrase input)
  (cond ((null phrase) t)
        ((null input) nil)
        ((keywordp (first phrase))
         (phrase-possible-p (rest phrase) (rest input)))
        (t (word-possible-p phrase input))))

(defun word-possible-p (phrase input)
  (let ((l (member (first phrase) input)))
    (and (not (null l))
         (phrase-possible-p (rest phrase) (rest l)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MATCH-CP-LIST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (MATCH-CP-LIST concept-phrase-list input) => list of parses
;;;   Returns all the successful parses, applying the concept phrases
;;;   to the input.

;;; (MATCH-CP-P concept-phrase input) => list of parses
;;;   Returns all the parses when concept-phrase is matched against
;;;   the input. 
;;;
;;; A phrase may contain words or role names. Words match directly.
;;; Role names require a recursive check for concept phrases that 
;;; could fill that role and that match the input.

;;; *cp-list* is dynamically bound to the static list of
;;; possible concept phrases; used by match-role
(defvar *cp-list*)

(defun match-cp-list (cp-list input)
  (let ((*cp-list* cp-list))
    (loop for cp in cp-list
          append (match-cp cp input))))

(defun match-cp (cp input)
  (match-phrase (cp-phrase cp) input (cp-concept cp)))

(defun match-phrase (phrase input concept &optional bindings used)
  (cond
    ((null phrase)      ;; success
     (list (make-parse :concept concept :bindings bindings 
                       :used used :input input)))
    ((null input) nil)  ;; failure
    (t (match-item (first phrase) (rest phrase)
                   input concept bindings used))))

(defun match-item (item phrase input concept bindings used)
  (cond
    ((keywordp item)
     (match-role item phrase input concept bindings used))
    (t (match-word item phrase input concept bindings used))))
    
(defun match-role (role phrase input concept bindings used)
  (let ((constraint (inherit-filler concept role)))
    (loop for cp in *cp-list*
          when (abstp constraint (cp-concept cp))
          append (continue-parses
                  (match-cp cp input) role phrase concept bindings used))))

(defun continue-parses (parses role phrase concept bindings used)
  (loop for parse in parses
        append (match-phrase phrase (parse-input parse) concept
                             (add-parse-binding role parse bindings)
                             (append used (parse-used parse)))))

(defun add-parse-binding (role parse bindings)
  (cons (list role (cons (parse-concept parse)
                         (parse-bindings parse)))
        bindings))

(defun match-word (word phrase input concept bindings used)
  (let ((l (member word input)))
    (and (not (null l))
         (match-phrase phrase (rest l) concept bindings (cons word used)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GET-BEST-PARSES
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (GET-BEST-PARSES parse-list) => parses
;;;   Returns the best parses in parse-list. 
;;;
;;; Parses are compared on amount of input matched (the more,
;;; the better), and, secondarily, by the specificity of concept
;;; identified (the more specific, the better). All parses that
;;; equally good and better than any others are returned as
;;; best parses.
;;;
;;; These are not very good rules. For example, for parses of
;;; (catch kangaroo rat), (m-catch-animal :object m-kangaroo-rat)
;;; is best, but (m-kangaroo-rat) and (m-catch-animal :object m-rat)
;;; are equivalent, because both use two input words, and neither is
;;; more specific than the other.

(defun get-best-parses (parses)
  (if (null (cdr parses))
      parses
    (reduce #'better-parses (cdr parses)
            :initial-value (list (car parses)))))

(defun better-parses (parses parse)
  (cond ((better-parse-p parse (car parses))
         (list parse))
        ((better-parse-p (car parses) parse)
         parses)
        (t (cons parse parses))))
  
(defun better-parse-p (parse1 parse2)
  (let ((parse1-len (length (parse-used parse1)))
        (parse2-len (length (parse-used parse2))))
    (or (> parse1-len parse2-len)
        (and (= parse1-len parse2-len)
             (strict-abstp (parse-concept parse2)
                           (parse-concept parse1))))))

(defun strict-abstp (abst spec)
  (and (not (eql abst spec))
       (abstp abst spec)))

(provide "parse")
