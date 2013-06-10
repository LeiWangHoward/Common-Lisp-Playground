(in-package :cs325-user)

;;; Change Log
;;; 10-15-2011 CKR removed ad hoc file loading in favor of QuickLisp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sample server-side code for xml-rpc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (include "xml-rpc-utils")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Begin pretend application code. This would normally be
;;; defined in other files.

(defpackage :logic
  (:use :common-lisp)
  (:export #:get-proof))

(in-package :logic)

;;; (GET-PROOF claim given) => a proof or nil
;;;   - claim: a logical proposition symbol, e.g., P or R
;;;   - given: a list of propositions and implications, e.g.,
;;       (P (-> P Q) (-> Q R))
;;;   - proof: a list of the assertions and implications that
;;;        prove claim, if possible, or NIL
;;;
;;; (GET-PROOF 'P '(P (-> P Q) (-> Q R))) => (P)
;;; (GET-PROOF 'Q '(P (-> P Q) (-> Q R))) => (P (-> P Q))
;;; (GET-PROOF 'Q '(P (-> P Q) (-> Q R))) => (P (-> P Q) (-> Q R))
;;; (GET-PROOF 'Q '((-> P Q) (-> Q R))) => NIL

(defun get-proof (claim given)
  (if (member claim given)
      (list claim)
    (some #'(lambda (form)
              (and (consp form)
                   (name= '-> (first form))
                   (eql claim (third form))
                   (let ((proof (get-proof (second form) given)))
                     (if (null proof) nil (cons form proof)))))
          given)))

;;; Needed because -> from another package is not equal to logic:->
(defun name= (x y)
  (and (symbolp x) (symbolp y) (string= (symbol-name x) (symbol-name y))))

;;; End of the pretend application code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cs325-user)

;;; Define some XML-RPC functions.
;;;
;;; An XML-RPC function is any function defined in the 
;;; S-XML-RPC-EXPORTS package. It does not need to be
;;; exported.
;;; By convention, XML-RPC method names use Java-style
;;; dotted camelCase, so for Lisp we need |...|.


;;; Functions with no arguments, or just numbers and strings
;;; are easy.

(defun s-xml-rpc-exports::|lisp.GCD| (m n)
  (gcd m n))

(defun s-xml-rpc-exports::|lisp.getTime| ()
  (multiple-value-list (get-decoded-time)))


;;; Functions that take symbolics arguments need to intern
;;; the strings XML-RPC sends into the appropriate package.
;;; To avoid accumulating random symbols from remote clients,
;;; we create a temporary package that is deleted after
;;; processing.

(defun s-xml-rpc-exports::|lisp.getProof| (claim given)
  (with-temp-package (pkg)
    (logic:get-proof (intern-strings claim pkg)
                     (intern-strings given pkg))))



;;; Evaluate the following to start the server.
;;;
;;; (setq *xml-server* (s-xml-rpc:start-xml-rpc-server :port 8080))
;;; 
;;; To stop the server
;;;
;;; (s-xml-rpc:stop-server *xml-server*)
