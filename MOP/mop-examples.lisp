;;; A toy MOP memory with examples referred
;;; to by various EECS 325 course materials.
;;;
;;; Note that various examples use different
;;; conventions (or none) to distinguish concepts
;;; from individuals.
;;;
;;; Updates
;;; 11/21/08 created mop-examples package [CKR]
;;; 02/29/08 merged three example files [CKR]
;;; 03/02/04 added Wilbur and Jumbo [CKR]

(defpackage #:mop-examples
  (:use :mops))

(in-package #:cs325-user)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Clyde the white elephant
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Clyde gets sick from eating peanuts.

(defmop mammal (animal)
        :numHearts 1
        :numEyes 2)

(defmop elephant (mammal)
        :color gray)

(defmop pig (mammal)
        :color pink)

(definstance clyde-1 (elephant)
             :name clyde
             :age 15
             :color white)

(definstance jumbo-1 (elephant)
             :name jumbo
             :age 40)

(definstance wilbur-1 (pig)
             :name wilbur
             :age 2)

(definstance event-1 (event)
             :actor clyde-1
             :action ingest
             :object peanuts-1)

(definstance state-1 (state)
             :actor clyde-1
             :state nauseous)

(definstance causal-1 (causal)
             :ante event-1
             :conse state-1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Inheritance examples
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Note that concepts can be defined in any order 
;;; or not defined at all.

(defmop dog (animal) :legs 4 :skin furred)

(defmop buddy (chihuahua pet) :age 3)

(defmop chihuahua (dog small-thing) :brain nil)

(defmop pet (animal) :owner human)

(defmop animal (living-thing))

(defmop living-thing (thing))

(defmop small-thing (thing) :size small)

;;; The pedalo boat inheritance example from
;;; Ducournau et al.

(defmop boat (thing))

(defmop day-boat (boat) :navzone 5)

(defmop wheel-boat (boat) :navzone 100)

(defmop engineless-boat (day-boat))

(defmop small-multi-hull-boat (day-boat))

(defmop pedal-wheel-boat (engineless-boat wheel-boat))

(defmop small-catamaran (small-multi-hull-boat))

(defmop pedalo (pedal-wheel-boat small-catamaran))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Generalization example
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; This particular content was inspired by Creanimate
;;; which used MOPs to describe short video clips
;;; of animal behavior.

(defmop m-mammal (m-animal))
(defmop m-fish (m-animal))

(defmop m-gillfish (m-fish))
(defmop m-walrus (m-mammal))

(defmop m-head-part (m-body-part))
(defmop m-tusk (m-head-part))
(defmop m-gill-cover (m-head-part))

(defmop m-lift-self (m-action))

(defmop m-move (m-behavior))
(defmop m-survive-drought (m-behavior))

(defmop i-walrus-lift-with-tusks (m-animal-motion-example)
  :animal m-walrus
  :feature m-tusk
  :action m-lift-self-with-tusks
  :behavior m-move)

(defmop m-lift-self-with-tusks (m-lift-self)
  :inst m-move-tusks)

(defmop m-move-tusks (m-move-body-part)
  :object m-tusk)

(defmop i-gillfish-climb-trees (m-animal-motion-example)
  :animal m-gillfish
  :feature m-gill-cover
  :action m-lift-self-with-gill-covers
  :behavior m-survive-drought)

(defmop m-lift-self-with-gill-covers (m-lift-self)
  :inst m-move-gill-covers)

(defmop m-move-gill-covers (m-move-body-part)
  :object m-gill-covers)


(provide "mop-examples")
