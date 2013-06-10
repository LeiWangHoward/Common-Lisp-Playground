(in-package :ddr-tests)

;;; This is a base set of rules for doing planning
;;; with the deductive retriever. It generalizes the
;;; ideas in the Monkey and Bananas example in several
;;; ways:
;;;
;;;  - It uses CONS/NIL instead DO/DONE to represent plans, 
;;;    so that you can use MEMBER and APPEND rules to manipulate
;;;    plans.
;;;  - It provides two general PLAN-FOR rules. New ones
;;;    shouldn't be needed.
;;;  - It separates the rules for how actions change state
;;;    from the rules for which actions are best for achieving
;;;    a desired state.
;;;
;;; There are three predicates used:
;;;
;;;   (PLAN-FOR start-state goal-state action-list)
;;;      The given actions lead from start-state
;;;      to goal-state. Action-list is either nil or
;;;      (CONS action action-list). The rules for this
;;;      predicate are in *GENERAL-PLANNING-KB*.
;;;
;;;   (ACTION-FOR start-state goal-state action)
;;;      Action is to be considered for getting from
;;;      start-state to goal-state. Action can be
;;;      a symbol like CLIMB-BOX or complex term
;;;      like (PUSH-BOX WINDOW CENTER). You define
;;;      rules for ACTION-FOR.
;;;
;;;   (RESULTS start-state end-state action)
;;;      end-state results from doing action in
;;;      start-state. You define rules for RESULTS.
;;;
;;; How to use:
;;;   - Define lists of rules for different predicates, e.g.,
;;;     *APPEND-KB*, *MEMBER-KB*, *ROOMS-KB*, ...
;;;   - Combine the ones you need with *GENERAL-PLANNING-KB* to
;;;     create a knowledge base for testing, like this:
;;;
;;;     (INIT-KB *GENERAL-PLANNING-KB* *ROOM-KB* *SHAKEY-1-KB*)
;;;     (RUN-TESTS SHAKEY-1)
;;;
;;; The Monkey and Bananas problem is reimplementd using 
;;; *GENERAL-PLANNING-KB* below, to show you how it works. You 
;;; don't need those rules for Shakey, just *GENERAL-PLANNING-KB*.

(defparameter *general-planning-kb*
  '(
    ;; No action needed if start = goal
    (plan-for ?goal ?goal nil)
    
    ;; (cons action actions) is a plan for goal from state1
    ;; if action is an appropriate action that leads to
    ;; state2, and actions lead from state2 to goal. 
    (<- (plan-for ?state1 ?goal (cons ?action ?actions))
        (action-for ?state1 ?goal ?action)
        (results ?state1 ?state2 ?action)
        (plan-for ?state2 ?goal ?actions))
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monkey and Bananas
;;;
;;; We represent monkey and banana problem states with
;;; the functional term (not predicate!):
;;;
;;;   (mb-state monkey-loc box-loc)
;;;
;;; Box can be at the DOOR, WINDOW or CENTER or room. 
;;; Monkey can be those places or on top of box. The
;;; goal state has the form
;;;
;;;   (mb-state box-top bananas-loc)
;;;
;;; i.e., the box has to be under the bananas and the
;;; monkey has to be on the box, e.g., 
;;;
;;; (ask '(plan-for (mb-state door window)
;;;                 (mb-state box-top center)
;;;                 ?actions))

;;; Variable naming conventions:
;;;   mloc - monkey location
;;;   bloc - box location
;;;   gloc - goal (bananas) location

(defparameter *monkey-kb*
  '(
    ;; ACTION RESULT RULES
    
    ;; climb-box changes the monkey's location from
    ;; a room location to the top of the box.
    ;;
    ;; The monkey has to be at the box.
    (results (mb-state ?bloc ?bloc)
             (mb-state box-top ?gloc)
             climb-box)
    
    ;; push-box changes the location of both the monkey
    ;; and the box.
    ;;
    ;; The monkey has to be at the box.
    (results (mb-state ?bloc1 ?bloc1)
             (mb-state ?bloc2 ?bloc2)
             (push-box ?bloc1 ?bloc2))
        
    ;; walk changes the location of the monkey.
    (results (mb-state ?mloc1 ?bloc)
             (mb-state ?mloc2 ?bloc)
             (walk ?mloc1 ?mloc2))
    
    
    ;; ACTION SELECTION RULES 
    ;;
    ;; These rules are needed to avoid endless loops and wasted
    ;; search. These rules omit many preconditions that don't
    ;; arise in this specific combination of rules.
    
    ;; climb-box if box under bananas, and monkey not
    ;; not on the box already
    (<- (action-for (mb-state ?gloc ?gloc)
                    (mb-state box-top ?gloc)
                    climb-box))
    
    ;; push-box to bananas if monkey at box, and
    ;; box not at bananas, 
    (<- (action-for (mb-state ?bloc ?bloc)
                    (mb-state ?mloc ?gloc)
                    (push-box ?bloc ?gloc))
        (different ?bloc ?gloc))
    
    ;; walk to box if not at or on box
    (<- (action-for (mb-state ?mloc ?bloc)
                    (mb-state ?mloc-2 ?gloc)
                    (walk ?mloc ?bloc))
        (different ?mloc ?bloc)
        (different ?bloc ?gloc))

    ))


;; A room with 3 locations, plus the top of the box.

(defparameter *simple-room-kb*
  '(
    ;; If x is not y, then y is not x.
    (-> (different ?x ?y) (different ?y ?x))
   
    ;; Every location is different from every other
    ;; location.
    ;;
    ;; Important! box-top is not listed as a location,
    ;; so that it will not be generated as an option
    ;; during search. If it were, (different x box-top)
    ;; would need to be added to the rules above to
    ;; avoid endless loops.
    (different window center)
    (different window door)
    (different center door)
   ))

(define-test monkey
  (init-kb *general-planning-kb* *monkey-kb* *simple-room-kb*)
  (assert-true
   (ask '(plan-for (mb-state box-top center)
                   (mb-state box-top center)
                   ?actions)))
  (assert-true
   (ask '(plan-for (mb-state center center)
                   (mb-state box-top center)
                   ?actions)))
  (assert-true
   (ask '(plan-for (mb-state window window)
                   (mb-state box-top center)
                   ?actions)))
  (assert-true
   (ask '(plan-for (mb-state door window)
                   (mb-state box-top center)
                   ?actions)))
  (assert-false
   (ask '(plan-for (mb-state box-top window)
                   (mb-state box-top center)
                   ?actions)))
  )
