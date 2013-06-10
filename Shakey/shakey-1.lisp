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

(defparameter *shakey-1-kb*
  '(
    ;; ACTION RESULT RULES
   
    ;; push-box changes the location of shakey-1 and the box.
    ;; Shakey-1 has to be at the box.
    (results (v1-state ?bloc-1 ?bloc-1)
             (v1-state ?bloc-2 ?bloc-2)
             (push-box ?bloc-1 ?bloc-2))
        
    ;; move-to changes the location of shakey-1.
    (results (v1-state ?sloc-1 ?bloc)
             (v1-state ?sloc-2 ?bloc)
             (move-to ?sloc-2))
    
    ;; ACTION SELECTION RULES

    ;; push-box to the room through hall if box not in the final destination
    (<- (action-for (v1-state ?bloc ?bloc)
                    (v1-state ?sloc ?gloc)
                    (push-box ?bloc ?gloc))
        (different ?bloc ?gloc)
        (connect ?bloc ?gloc))
   
    ;; push-box to hall if box at a room other than he destination
    (<- (action-for (v1-state ?bloc ?bloc)
                    (v1-state ?sloc ?gloc)
                    (push-box ?bloc hall))
        (different ?bloc ?gloc)
        (not (connect ?bloc ?gloc)))

    ;; move-to another room if not at box location
    ;; shakey at hall
    (<- (action-for (v1-state ?sloc ?bloc);sloc
                    (v1-state ?sloc-2 ?gloc) ; 2 bloc
                    (move-to ?bloc))
        (different ?sloc ?bloc)
        (connect ?sloc ?bloc))

    ;; move-to hall if not at box location 
    ;; shakey at room 
    (<- (action-for (v1-state ?sloc ?bloc)
                    (v1-state ?sloc-2 ?gloc)
                    (move-to hall))
        (different ?sloc ?bloc)
        (not (connect ?sloc ?bloc)))
    ))


;; 4 locations, 3 rooms and 1 hall

(defparameter *room-kb*
  '( 
    (-> (different ?x ?y) (different ?y ?x))
    (-> (connect ?x ?y) (connect ?y ?x))
    (different room1 room2)
    (different room1 room3)
    (different room2 room3)
    (different room1 hall)
    (different room2 hall)
    (different room3 hall)
    (connect room1 hall)
    (connect room2 hall)
    (connect room3 hall)
   ))

