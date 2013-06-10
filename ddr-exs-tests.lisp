(in-package :ddr-tests)


#|
Copyright (c) 2007 Christopher K. Riesbeck

Permission is hereby granted, free of charge, to any person obtaining 
a copy of this software and associated documentation files (the "Software"), 
to deal in the Software without restriction, including without limitation 
the rights to use, copy, modify, merge, publish, distribute, sublicense, 
and/or sell copies of the Software, and to permit persons to whom the 
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included 
in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS 
OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL 
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR 
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, 
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR 
OTHER DEALINGS IN THE SOFTWARE.
|#

;;; Test cases for the exercises in ddr-exs.html

;;; UPDATES
;;; 10/15/10 Changed tests to load rule sets automatically [CKR]
;;; 10/13/10 Added test to MEMBER [CKR]
;;; 09/21/09 Moved Shakey tests to separate file and package [CKR]
;;; 02/09/08 Added a test to MEMBER [CKR]
;;; 02/07/08 Added another test to ALL-DIFFERENT (from Jeremy Meisel) [CKR]
;;; 02/02/08 Added tests to MEMBER [CKR]

;;; MEMBER

(declaim (special *member-kb*))

(define-test member
   (init-kb *member-kb*)
  
  (assert-false (ask '(member a nil)))
  (assert-true (ask '(member a (cons a nil))))
  (assert-false (ask '(member nil (cons a nil))))
  (assert-true (ask '(member b (cons a (cons b (cons c nil))))))
  (assert-true (ask '(member c (cons a (cons b (cons c nil))))))
  (assert-false (ask '(member d (cons a (cons b (cons c nil))))))
  (assert-false (ask '(member nil nil)))
  (assert-false (ask '(member a a)))
  (assert-false (ask '(member a (cons (cons a nil) (cons b nil)))))
  )


;;; ALL-DIFFERENT

(declaim (special *all-different-kb*))

(define-test all-different
  (init-kb *all-different-kb*)
  
  (assert-false (ask '(different a b)))
  (tell '(all-different nil))
  (assert-false (ask '(different a b)))
  (tell '(all-different (cons a (cons b (cons c nil)))))
  (assert-true (ask '(different a b)))
  (assert-true (ask '(different a c)))
  (assert-true (ask '(different b a)))
  (assert-true (ask '(different b c)))
  (assert-true (ask '(different c a)))
  (assert-true (ask '(different c b)))
  (assert-false (ask '(different a a)))
  (assert-false (ask '(different a d)))
  )

;;; MAP COLORING

(declaim (special *map-color-kb*))

(define-test color-map1
  (init-kb *map-color-kb*)

  (assert-equal '((colors-for map1 red blue green yellow))
                (ask '(colors-for map1 red blue green ?d)))
  (assert-equal 2 (length (ask '(colors-for map1 red blue ?c ?d))))
  (assert-equal 24 (length (ask '(colors-for map1 ?a ?b ?c ?d))))
  (assert-equal nil (ask '(colors-for map1 red blue green red)))
  )

(define-test color-map2
  (init-kb *map-color-kb*)

  (assert-equal '((colors-for map2 red blue green blue yellow))
                (ask '(colors-for map2 red blue green ?d ?e)))
  (assert-equal 2 (length (ask '(colors-for map2 red blue ?c ?d ?e))))
  (assert-equal 24 (length (ask '(colors-for map2 ?a ?b ?c ?d ?e))))
  (assert-equal nil (ask '(colors-for map2 red blue green yellow ?e)))
  )

(define-test color-map3
  (init-kb *map-color-kb*)

  (assert-equal '((colors-for map3 red blue green yellow green blue))
                (ask '(colors-for map3 red blue green yellow ?e ?f)))
  (assert-equal 1 (length (ask '(colors-for map3 red blue green ?d ?e ?f))))
  (assert-equal 24 (length (ask '(colors-for map3 ?a ?b ?c ?d ?e ?f))))
  (assert-equal nil (ask '(colors-for map3 red blue green blue ?e ?f)))
  )

