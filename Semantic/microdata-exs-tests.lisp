(defpackage microdata-tests
  (:use :common-lisp :lisp-unit :microdata)
  )

(in-package :microdata-tests)

;;; Some simple tests for microdata reader exercises
;;;  http://www.cs.northwestern.edu/academics/courses/325/exercises/semweb-exs.php

;;; Update history:
;;;
;;; 11-28-11 add 3 word examples to CAMELIZE and HYPHENATE [CKR]

;;; CAMELIZE

(define-test camelize
  (assert-equal "job" (camelize "job"))
  (assert-equal "Job" (camelize "job" t))
  (assert-equal "jobPosting" (camelize "job-posting"))
  (assert-equal "BookFormatType" (camelize "book-format-type" t))
  (assert-equal "JobPosting" (camelize "job-posting" t))
)

;;; HYPHENATE

(define-test hyphenate
  (assert-equal "JOB" (hyphenate "job"))
  (assert-equal "JOB" (hyphenate "Job"))
  (assert-equal "JOB" (hyphenate "Job" :upper))
  (assert-equal "job" (hyphenate "Job" :lower))
  (assert-equal "JOB-POSTING" (hyphenate "jobPosting"))
  (assert-equal "JOB-POSTING" (hyphenate "JobPosting"))
  (assert-equal "BOOK-FORMAT-TYPE" (hyphenate "BookFormatType"))
  (assert-equal "URL" (hyphenate "URL"))
  (assert-equal "GET-ID" (hyphenate "getID"))
  )

;;; READ-MICRODATA

;;; This is particularly tricky to test because we need to
;;; make sure symbols are being added to the org.schema
;;; package without incidentally adding those symbols
;;; in the test code.

;;; So we define MD-EQUAL that compares two lists, comparing
;;; symbols by name only, then checking the second
;;; list to verify that any non-NIL non-keyword
;;; symbols are external symbols from org.schema

(defpackage org.schema)
(defparameter *md-pkg* (find-package "ORG.SCHEMA"))

(defun md-equal (x y)
  (cond ((consp x)
         (and (consp y) (every #'md-equal x y)))
        ((or (null x) (not (symbolp x)) (keywordp x))
         (equal x y))
        ((symbolp y) 
         (and (eql (symbol-name x) (symbol-name y))
              (external-symbol-p y *md-pkg*)))
        (t nil)))

(defun external-symbol-p (sym pkg)
  (or (and (eql (symbol-package sym) pkg)
           (eql (nth-value 1 (find-symbol (symbol-name sym) pkg))
                :external))
      (fail "~S not external symbol in ~A" 
            sym (package-name pkg))))

  
(define-test read-microdata
  (assert-equality 'md-equal '() (read-microdata  "<div>something</div>"))
  (assert-equality 'md-equal '((item))
     (read-microdata "<div itemscope>something</div>"))
  (assert-equality 'md-equal'((job-posting))
     (read-microdata 
      "<div itemscope itemtype=\"http://schema.org/JobPosting\">something</div>"))
  )
