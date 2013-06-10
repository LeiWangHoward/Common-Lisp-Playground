(in-package :cl-user)

;;; Suggested start up file for EECS 325
;;; Platform: LispWorks on Windows and MacOS

;;; Update History
;;;
;;; 09/23/2011 Clarified comments on directory changing [CKR]

#-quicklisp
(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp" (user-homedir-pathname))))
    (when (probe-file quicklisp-init)
      (load quicklisp-init))))

;;; Set the depth of trace output to a large but finite level.
(setq hcl:*trace-print-level* 10)

;;; Turn off backup files
(setf (editor:variable-value 'editor::backups-wanted) nil)

;;; Switches the default directory to the one containing this file
(hcl:change-directory
 (make-pathname :directory (pathname-directory *load-truename*)))


(ql:quickload "aserve")
(ql:quickload "webactions")

;;; Load cs325.lisp to create the cs325 package.

(eval-when (:compile-toplevel :load-toplevel :execute)
  (load "cs325.lisp"))