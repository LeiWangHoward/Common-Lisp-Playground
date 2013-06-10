;;; Example Webactions project
;;;
;;; Uses the following toy packages:
;;;  - date-time.lisp 
;;;  - vote.lisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; the webapp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:execute :load-toplevel :compile-toplevel)
  (cs325:include "clp-exts" "vote-demo/date-time" "vote-demo/vote")
  )

(defpackage #:vote-demo
  (:use #:common-lisp #:net.aserve #:net.html.generator #:clp-exts #:time #:vote))

(in-package :vote-demo)

;;; Set *vote-home* to the directory containing this file
(defparameter *vote-home*
  (let ((load-name (or *compile-file-truename* *load-truename*)))
    (namestring
     (make-pathname :host (pathname-host load-name)
                    :device (pathname-device load-name)
                    :directory (pathname-directory load-name)))))

;;; inspired by the food voting example from webactions documentation. 
;;; major changes include:
;;;
;;;  - thanks.clp replaced with results.clp
;;;  - results.clp has a table of votes, generated with clp-foreach
;;;  - all posts are followed by a redirect
;;;  - all pages except login are protected by action-check-sign-in
;;;  - action-check-sign-in stores page you were trying to get to when you
;;;      were re-routed to login.clp
;;;  - action-sign-in looks to see what page you should go to after login

(webaction-project
 "vote-demo"
 :project-prefix "/vote-demo/"
 :destination *vote-home*
 :index "show/login"
 :map
 '(;; return a resource
   ("show/login" "login.clp")
   ("show/choice" action-check-sign-in "choice.clp")
   ("show/results" action-check-sign-in "results.clp")
   
   ;; respond to a post
   ("do/signin" action-sign-in "choice.clp" (:redirect t))
   ("do/vote" action-check-sign-in action-vote "results.clp" (:redirect t))
   ))

(defun action-check-sign-in (req resp)
  (declare (ignore resp))
  (setf (websession-variable (websession-from-req req) "error") nil)
  (cond ((websession-variable (websession-from-req req) "name")
         :continue)
        (t
         (setf (request-variable-value req "after-login") 
           (net.uri:uri-path (request-uri req)))
         "show/login")))

(defun action-sign-in (req resp)
  (declare (ignore resp))
  (let ((name (request-query-value "name" req))
        (password (request-query-value "password" req))
        (after (request-query-value "after-login" req)))
    (cond ((valid-login-p name password)
           (setf (websession-variable (websession-from-req req) "name") name)
           (if (empty-arg-p after)
               :continue
             after))
          (t
           (setf (websession-variable (websession-from-req req) "error")
             "name and/or password are invalid")
           "show/login"))))

(defun action-vote (req resp)
  (declare (ignore resp))
  (let ((food (request-query-value "food" req))
        (name (websession-variable (websession-from-req req) "name")))
    (add-vote name food)
    :continue))

(defun valid-login-p (name password)
  (and (not (empty-arg-p name))
       (string= password (reverse name))))

;;; CLP tags

(def-clp-function vote_show-favorite (req resp args body)
  (declare (ignore resp args body))
  (html
   (:princ-safe 
    (or (get-vote (websession-variable (websession-from-req req) "name"))
        "not given"))))
