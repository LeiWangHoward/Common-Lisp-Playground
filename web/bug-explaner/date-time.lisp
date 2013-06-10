;;; A toy date-time formatting package for WebActions demo


(defpackage #:time
  (:use :common-lisp)
  (:export #:get-date-string #:get-month-string 
           #:get-time-string #:get-timestamp-string))

(in-package #:time)

(defun get-date-string (&optional (utime (get-universal-time)))
  (multiple-value-bind (second minute hour date month year)
      (decode-universal-time utime)
    (format nil "~a ~2,'0d, ~d" (get-month-string month) date year)))

(defun get-time-string (&optional (utime (get-universal-time)))
  (multiple-value-bind (second minute hour)
      (decode-universal-time utime)
    (format nil "~d:~2,'0d ~a" 
      (mod hour 12) minute (if (>= hour 13) "PM" "AM"))))

(defun get-timestamp-string (&optional (utime (get-universal-time)))
  (format nil "~A ~A" (get-date-string utime) (get-time-string utime)))

(defun get-month-string (month)
  (nth (1- month)
       '("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")))
