;;;     (INIT-KB *GENERAL-PLANNING-KB* *ROOM-KB* *SHAKEY-1-KB*)
;;;     (RUN-TESTS SHAKEY-1)
(in-package #:shakey-tests)

(defparameter *general-planning-kb*
  '(
    ;; No action needed if start = goal
    (plan-for ?goal ?goal nil)
    (<- (plan-for ?state1 ?goal (cons ?action ?actions))
        (action-for ?state1 ?goal ?action)
        (results ?state1 ?state2 ?action)
        (plan-for ?state2 ?goal ?actions))
    ))

;;; Variable naming conventions:
;;;   sloc - shakey-1 location
;;;   bloc - box location
;;;   gloc - goal (destination)

(defparameter *shakey-2-kb*
  '(
    ;; ACTION RESULT RULES
   
    ;; push-box changes the location of shakey-2 and the box.
    ;; Shakey-2 has to be at the box.
    (results (v2-state ?bloc-1 ?bloc-1 ?unlocked)
             (v2-state ?bloc-2 ?bloc-2 ?unlocked)
             (push-box ?bloc-1 ?bloc-2))
        
    ;; move-to changes the location of shakey-2.
    (results (v2-state ?sloc-1 ?bloc ?unlocked)
             (v2-state ?sloc-2 ?bloc ?unlocked)
             (move-to ?sloc-2))
    
    ;; unlock a room
    (results (v2-state ?sloc-1 ?bloc ?unlocked)
             (v2-state ?sloc-1 ?bloc (cons ?room ?unlocked))
             (unlock ?room))
    
    ;; ACTION SELECTION RULES

    ;; unlock room if room locked, and shakey at hall
    ;;(<- (action-for (v2-state ?sloc ?bloc ?unlocked)
    ;;                (v2-state ?sloc-2 ?bloc-2 (cons ?room ?unlocked))
    ;;                (unlock ?room))
    ;;    ;(different ?sloc ?room)
    ;;    (not (member ?room ?unlocked))
    ;;    (different ?room hall);hall needn't to be unlocked
    ;;    (connect ?sloc ?room))
    ;;    ;(not (different ?bloc-2 ?room)))

    ;; push-box to the room from hall if box not in the final destination, and room is unlocked
    (<- (action-for (v2-state ?bloc ?bloc ?unlocked)
                    (v2-state ?sloc room1 ?unlocked-2) ;;room1 to ?gloc
                    (push-box ?bloc room1))
        ;(different ?bloc ?gloc)
        (member room1 ?unlocked)
        (connect ?bloc room1))
   
    ;; push-box to hall if box at a room other than he destination, since shakey in the room, 
    ;; the room shall be considered unlocked
    (<- (action-for (v2-state ?bloc ?bloc ?unlocked)
                    (v2-state ?sloc ?gloc ?unlocked-2)
                    (push-box ?bloc hall))
        (different ?bloc ?gloc)
        (not (connect ?bloc ?gloc)))

    ;; move-to another room if not at box location
    ;; shakey at hall
    (<- (action-for (v2-state ?sloc ?bloc ?unlocked)
                    (v2-state ?sloc-2 ?gloc ?unlocked-2)
                    (move-to ?bloc))
        ;(different ?sloc ?bloc)
        (member ?bloc ?unlocked)
        (connect ?sloc ?bloc))

    ;; move-to hall if not at box location 
    ;; shakey at room 
    (<- (action-for (v2-state ?sloc ?bloc ?unlocked)
                    (v2-state ?sloc-2 ?gloc ?unlocked-2)
                    (move-to hall))
        (different ?sloc ?bloc)
        (not (connect ?sloc ?bloc)))

    ;; unlock room 
    (<- (action-for (v2-state ?sloc ?bloc ?unlocked)
                    (v2-state ?sloc-2 ?bloc-2 ?unlocked-2)
                    (unlock ?room))
        ;(different ?sloc ?room)
        ;(not (different ?bloc ?room))
        (not (member ?room ?unlocked))
        (different ?room hall);hall needn't to be unlocked
        (connect ?sloc ?room))
))

;; 4 locations, 3 rooms and 1 hall

(defparameter *room-kb*
  '( 
    (-> (different ?x ?y) (different ?y ?x))
    (-> (connect ?x ?y) (connect ?y ?x))
    (different room1 room2)
    ;(different room1 room3)
    (different room1 hall)
    ;(different room2 room3)
    (different room2 hall)
    ;(different room3 hall)
    (connect room1 hall)
    (connect room2 hall)
    ;(connect room3 hall)
   ))

(defparameter *member-kb*
  '(
    (member ?x (cons ?x nil))
    (<- (member ?x (cons ?l3 (cons ?l1 ?l2))) (member ?x (cons ?l1 ?l2))) ;Walk the list when not find x
    (<- (member ?x (cons ?x (cons ?l1 ?l2))) (member ?x (cons ?x nil))) ;when find x, return true all the way back
    ))

#|
(define-test shakey-2  
  (init-kb *shakey-2-kb* *general-planning-kb* *room-kb* *member-kb*)
  
  ;; Test with rooms unlocked
  (assert-equal '(nil)
                (ask '(plan-for (v2-state room1 room1 nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box hall room1) nil))
                (ask '(plan-for (v2-state hall hall (cons room1 nil))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box room2 hall) (cons (push-box hall room1) nil)))
                (ask '(plan-for (v2-state room2 room2 (cons room1 (cons room2 nil)))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  (assert-equal '((cons (move-to hall)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (push-box hall room1) nil)))))
                (ask '(plan-for (v2-state room1 room2 (cons room1 (cons room2 nil)))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  
  ;; Test with the room with the box locked
  (assert-equal '((cons (move-to hall)
                      (cons (unlock room2)
                          (cons (move-to room2)
                              (cons (push-box room2 hall)
                                  (cons (push-box hall room1) nil))))))
                (ask '(plan-for (v2-state room1 room2 (cons room1 nil))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  
  ;; Test with the goal room locked, robot and box in hall
  (assert-equal '((cons (unlock room1)
                      (cons (push-box hall room1) nil)))
                (ask '(plan-for (v2-state hall hall nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))

  ;; Test with the goal room locked, robot in hall, box in room2
  (assert-equal '((cons (move-to room2)
                      (cons (push-box room2 hall)
                          (cons (unlock room1)
                              (cons (push-box hall room1) nil)))))
                (ask '(plan-for (v2-state hall room2 (cons room2 nil))
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions)
                     '?actions))

  ;; Test with both rooms locked and the robot in the hall
  (assert-equal '((cons (unlock room2)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (unlock room1)
                                  (cons (push-box hall room1) nil))))))
                (ask '(plan-for (v2-state hall room2 nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))
  
  ;; Test with robot in locked room
  (assert-equal nil
                (ask '(plan-for (v2-state room1 room2 nil)
                                (v2-state ?rloc room1 ?unlocked)
                                ?actions) 
                     '?actions))

  )
|#