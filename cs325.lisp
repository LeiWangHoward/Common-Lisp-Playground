;;; Update history:
;;; 10-19-11 Change INCLUDE to take many files, not import packages [CKR]
;;; 09-24-11 Fixed defaulting bug in INCLUDE [CKR]
;;; 09-18-11 Added get-code-file to exports [CKR]
;;; 09-15-11 Split CS325 package off from CS325-USER [CKR]
;;; 08-13-11 Added CLP-EXTS to the included files [CKR]
;;; 08-01-11 Simplified INCLUDE, added REFRESH [CKR]
;;; 11-30-09 Restored remote-load code that got lost somehow [CKR]
;;; 11-06-09 Generalized GET-CODE-FILE [CKR]
;;; 09-22-09 Updated to remote-load files from EECS 325 site [CKR]
;;; 02-29-08 Updated to load merged mop-examples.lisp [CKR]
;;; 02-28-08 Updated to load just new mops.lisp [CKR]
;;; 02-15-05 Renamed module variable to avoid conflict with ASDF [CKR]
;;; 12-01-04 New include with integrated use-package and pathname defaults [CKR]
;;; 02-04-03 Replaced require with include [CKR]

;;; AllegroServe or PortableAllegroServe must be installed
;;; already by your initialization file.
(eval-when (compile load eval)
  (require :aserve)
  (require :webactions)
  )

(defpackage #:cs325
  (:use #:common-lisp #:net.aserve #:net.html.generator)
  (:export #:get-code-file #:include #:refresh #:*cs325-home*)
  )

(in-package #:cs325)

;;(ql:quickload "cl-html-parse")

;;; Sets *CS325-HOME* to the directory where you keep CS325 code
;;; Sets *CS325-LIB-URL* to the URL for CS325 library code
;;; Sets *DEFAULT-PATHNAME-DEFAULTS* to CS325-HOME* so that
;;;   (PROBE-FILE relative-pathname) works. (Needed by Webactions.)

(defparameter *cs325-home*
  (let ((load-name (or *compile-file-truename* *load-truename*)))
    (namestring
     (make-pathname :host (pathname-host load-name)
                    :device (pathname-device load-name)
                    :directory (pathname-directory load-name)))))

(defparameter *cs325-defaults*
  (make-pathname :type "lisp" :defaults *cs325-home*))

(defparameter *cs325-lib-url* 
  "http://www.cs.northwestern.edu/academics/courses/325/programs/")



;;; (INCLUDE name1 name2 ...)
;;;   Loads the code files named if not already loaded, first
;;;   downloading from the 325 web site if not local.
;;;
;;; Returns:
;;;   list of the files newly loaded
;;;
;;; Details:
;;;   The list *MODULES* is used to track if name has been loaded.
;;;   Loads the compiled or source version, whichever is newer.
;;;   Code is loaded from the user's local CS325 directory.
;;;   Source is downloaded from the CS325 web site code directory.
;;;   Code is loaded into a package called name.
;;;   USE-PACKAGE is called to add the new package to the *PACKAGE*.
;;;
;;; Examples:
;;;   (include "tables" "mops")
;;;     If not already loaded, downloads tables.lisp and mops.lisp
;;;     from 325 web directory to 325 local directory, if no local
;;;     copy exists, then loads from the 325 directory.

(defun include (&rest names)
  (let ((loaded-files nil))
    (dolist (name names loaded-files)
      (unless (included-p name)
        (let ((file (get-code-file name *cs325-defaults*)))
          (cond ((null file)
                 (error "~S not found" name))
                (t
                 (load file)
                 (pushnew name *modules* :test #'equal)
                 (push file loaded-files))))))))

;;; (REFRESH name &key compile)
;;;   Downloads the code from the 325 web site and reloads into memory.
;;;   Recompiles first if :COMPILE is true. Default is false.
;;;
;;; Returns:
;;;   The code file loaded.

(defun refresh (name &key compile)
  (let* ((source (remote-load (merge-pathnames name *cs325-defaults*)))
         (compiled (if compile (compile-file source) nil)))
    (load (or compiled source))))


(defun get-code-file (name &optional (defaults (get-source-defaults)))
  (let ((source (merge-pathnames name defaults)))
    (get-newer-file (compile-file-pathname source) source)))

(defun get-source-defaults ()
  (merge-pathnames (make-pathname :type "lisp")
                   (or *compile-file-truename* *load-truename*)))

;;; Returns the name of the newer file. In case of a tie, returns source
;;; If neither exists, tries to download source from the remote site

(defun get-newer-file (compiled source)
  (with-open-file (stream1 compiled :if-does-not-exist nil)
    (with-open-file (stream2 source :if-does-not-exist nil)
      (cond ((and (null stream1) (null stream2))
             (remote-load source))
            ((null stream2) compiled)
            ((null stream1) source)
            ((> (file-write-date stream2)
                (file-write-date stream1))
             source)
            (t compiled)))))

(defun included-p (name)
  (member name *modules* :test #'equal))

(defun get-module-package (name)
  (or (find-package name)
      (find-package (string-upcase name))
      (find-package (string-downcase name))))

;;; REMOTE FILE LOADER

(defun remote-load (source)
  (let* ((url (get-remote-url (file-namestring source)))
         (content (get-response-content url)))
    (if (null content)
        (error "~S not found" url)
      (with-open-file (stream source :direction :output :if-exists :new-version)
        (when *load-verbose*
          (format t "~&; Downloading ~A" url)) 
        (write-string content stream)
        source))))

(defun get-remote-content (path &key (base-url *cs325-lib-url*))
  (get-response-content (get-remote-url path :base-url base-url)))

(defun get-remote-url (path &key (base-url *cs325-lib-url*))
  (concatenate 'string base-url path))

(defun get-response-content (url)
  (multiple-value-bind (content code headers uri)
      (net.aserve.client:do-http-request url)
    (declare (ignore uri headers))
    (if (= code 404) nil content)))

(defun get-response-header (url header)
  (cdr (assoc header (get-response-headers url))))

(defun get-response-headers (url)
  (multiple-value-bind (content code headers uri)
      (net.aserve.client:do-http-request url :method :head)
    (declare (ignore uri content))
    (if (= code 404) nil headers)))

(provide "cs325")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CS325-USER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (include "tables" "extend-match" "lisp-unit" "lisp-critic"
            "mops" "ddr"))

(defpackage #:cs325-user
  (:use #:common-lisp #:cs325 #:lisp-unit #:lisp-critic #:mops #:ddr))

;;; Load unit tests, critic rules, and Webaction extensions

(eval-when (:compile-toplevel :load-toplevel :execute)
  (include "clp-exts" "exercise-tests" "lisp-rules" "ddr-tests" "ddr-exs-tests"))

(format t "~&REMINDER: call (in-package #:cs325-user) first.~%")

(provide "cs325-user")