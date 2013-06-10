(in-package #:ddr-tests)

(defparameter *all-different-kb*
  '(
    (-> (all-different (cons ?l3 (cons ?l1 ?l2))) (all-different (cons ?l1 ?l2)) (all-different (cons ?l3 ?l1)) (all-different (cons ?l3 ?l2)))
    (-> (all-different (cons ?l1 ?l2)) (different ?l1 ?l2))
    (-> (different ?x ?y) (different ?y ?x))
    ))


#|
(defparameter *all-different-kb*
  '(
    ;(different ?x)
    ;(different ?x nil))
    ;(-> (different ?x ?y) (different ?y ?x))
    ;(<- (different ?x ?y) (different (cons ?x ?y)))
    ;(<- (different (cons ?x ?y)) (different (cons ?z (cons ?x ?y))))
    
    ;(different ?x (cons ?y nil))
    (-> (all-different (cons ?l3 (cons ?l1 ?l2))) (all-different (cons ?l1 ?l2)) (all-different (cons ?l3 ?l1)) (all-different (cons ?l3 ?l2)))
    ;(-> (all-different (cons ?l3 (cons ?l1 ?l2))) (all-different (cons ?l1 ?l2)) (different ?l3 ?l1) (different ?l3 ?l2))
    (-> (all-different (cons ?l1 ?l2)) (different ?l1 ?l2))
    (-> (different ?x ?y) (different ?y ?x))
    ))
|#

;;; ALL-DIFFERENT
#|
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
|#