(in-package #:ddr-tests)
(defparameter *horses-kb*
  '(
    ;; x is ancestor of y if x is a parent of y
    (<- (ancestor ?x ?y) (parent ?x ?y))
      
    ;; x is a horse if x is a descendant of a horse
    (<- (horse ?x) (ancestor ?y ?x) (horse ?y))
    ;;; x is a horse if x is a horse's ancestor
    ;;;(<- (horse ?x) (ancestor ?x ?y) (horse ?y))  
    ;; some real horses, not all real relationships
    (parent man-o-war war-admiral)
    (parent war-admiral seabiscuit)
    (parent seabiscuit kelso)
    (horse man-o-war)
    ))
