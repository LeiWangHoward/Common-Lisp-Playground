#|(defparameter *member-kb*
  '(
    (member ?x (cons ?x nil))
    ;;(<- (member ?x ?x) (member ?x (cons ?x nil))
    ;;(cons ?x ?lst)
    (<- (member ?x (cons ?l3 (cons ?l1 ?l2))) (member ?x (cons ?l1 ?l2)))
    (<- (member ?x (cons ?x (cons ?l1 ?l2)) (mamber ?x (cons ?x nil))))
    ))|#
(in-package #:ddr-tests)

(defparameter *member-kb*
  '(
    (member ?x (cons ?x nil))
    (<- (member ?x (cons ?l3 (cons ?l1 ?l2))) (member ?x (cons ?l1 ?l2))) ;Walk the list when not find x
    (<- (member ?x (cons ?x (cons ?l1 ?l2))) (member ?x (cons ?x nil))) ;when find x, return true all the way back
    ))
