
(defpackage #:shakey-tests
  (:use #:common-lisp #:lisp-unit #:ddr)
  )

(in-package :shakey-tests)


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

;;; Test cases for the exercises in shakey-exs.html


;;; UPDATES
;;; 10/15/10 Changed tests to load rule sets automatically [CKR]
;;; 09/21/09 Moved Shakey tests to separate file and package [CKR]

(declaim (special *shakey-1-kb* *shakey-2-kb* *shakey-3-kb*))

;;; SHAKEY 1.0

;;; Test cases for 1 box, no locks.
;;;
;;; The goal state is always (v1-state ?rloc room1), meaning
;;; the box has to end up in room1, and it doesn't matter where
;;; the robot ends up.


(define-test shakey-1 
  (init-kb *shakey-1-kb*)
  
  (assert-equal '(nil)
                (ask '(plan-for (v1-state room1 room1)
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box hall room1) nil))
                (ask '(plan-for (v1-state hall hall) 
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (push-box room2 hall)
                      (cons (push-box hall room1) nil)))
                (ask '(plan-for (v1-state room2 room2) 
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  (assert-equal '((cons (move-to hall)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (push-box hall room1) nil)))))
                (ask '(plan-for (v1-state room1 room2) 
                                (v1-state ?1 room1)
                                ?actions)
                     '?actions))
  )

;;; SHAKEY 2.0

;;; Test cases for 1 box with locks.
;;;
;;; The goal state is always
;;;
;;;   (v2-state ?rloc room1 ?unlocked)
;;;
;;; meaning the box has to end up in room1, and we don't care
;;; where the robot is or what rooms are unlocked.

(define-test shakey-2  
  (init-kb *shakey-2-kb*)
  
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

;;; SHAKEY 3.0

;;; Test cases for N boxes with locks, going to the same room.
;;;
;;; The goal state that stops the recursion is that the list
;;; of box locations is nil:
;;;
;;;   (v3-state ?rloc nil ?gloc ?unlocked)


(define-test shakey-3
  (init-kb *shakey-3-kb*)

  ;; Test already done case
  (assert-equal '(nil)
                (ask '(plan-for (v3-state ? nil ? ?)
                                ?actions)
                     '?actions))
  
  ;; Test with 1 box, all rooms locked, the robot in the hall
  (assert-equal '((cons (unlock room2)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (unlock room1)
                                  (cons (push-box hall room1) nil))))))
                (ask '(plan-for (v3-state hall (cons room2 nil) room1 nil)
                                ?actions)
                     '?actions))
  
  ;; Test with 2 boxes, all rooms locked, the robot in the hall
  (assert-equal '((cons (unlock room2)
                      (cons (move-to room2)
                          (cons (push-box room2 hall)
                              (cons (unlock room1)
                                    (cons (push-box hall room1) 
                                          (cons (move-to hall)
                                                (cons (unlock room3)
                                                      (cons (move-to room3)
                                                            (cons (push-box room3 hall)
                                                                  (cons (push-box hall room1)
                                                                        nil)))))))))))
                (ask '(plan-for (v3-state hall (cons room2 (cons room3 nil)) room1 nil)
                                ?actions)
                     '?actions))
  )
