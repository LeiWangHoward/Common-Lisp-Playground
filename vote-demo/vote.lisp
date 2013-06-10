;;; A toy vote tallying package for WebActions demo

(defpackage #:vote
  (:export #:add-vote #:clear-votes #:get-choices 
           #:get-vote #:get-voters #:get-vote-time))

(in-package #:vote)

(defvar *votes* nil)

(defun add-vote (name choice)
  (push (list name choice (get-universal-time)) *votes*))

(defun clear-votes ()
  (setq *votes* nil))

(defun get-choices ()
  (get-uniques (mapcar #'second *votes*)))

(defun get-vote (name) 
  (second (get-vote-data name)))

(defun get-vote-data (name)
  (assoc name *votes* :test #'equal))

(defun get-vote-time (name) 
  (third (get-vote-data name))) 

(defun get-voters () 
  (get-uniques  (mapcar #'first *votes*)))


(defun get-uniques (lst)
  (sort (remove-duplicates lst :test #'equal) #'string<))