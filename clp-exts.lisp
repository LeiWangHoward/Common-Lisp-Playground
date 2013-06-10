
(defpackage #:clp-exts
  (:use #:common-lisp #:net.aserve #:net.html.generator)
  (:export #:empty-arg-p #:lookup))

(in-package #:clp-exts)

;;; Additional CLP tags and utility functions
;;;
;;; Functions
;;; ---------
;;;
;;; (empty-arg-p string)
;;;   Returns true if string is null, empty or just whitespace
;;;
;;; Tags
;;; ----
;;;
;;; clp_eval, clp_foreach, clp_when, clp_choose, clp_otherwise
;;; See http://www.cs.northwestern.edu/academics/courses/325/readings/aserve-webactions.php


;;; (empty-arg-p str) true is str is NIL or a string with nothing but whitespace
(defun empty-arg-p (str) 
  (or (null str) (null (position-if-not #'whitespace-p str))))

(defun whitespace-p (x)
  (member x '(#\Space #\Tab #\Return #\Linefeed)))


;;; Additional CLP tags


;;; tags

(def-clp-function clp_eval (req resp args body)
  (let ((var (arg-value "var" args nil))
        (value (eval-el-string req 
                               (arg-value "value" args)
                               (arg-value "package" args "common-lisp"))))
    (if (null var)
        (and value (html (:princ-safe value)))
      (setf (request-variable-value req var) value))))

(def-clp-function clp_foreach (req resp args body)
  (let ((var (arg-value "var" args))
        (status-var (arg-value "status" args nil))
        (items (eval-el-string req 
                               (arg-value "items" args)
                               (arg-value "package" args "common-lisp"))))
    (loop 
      for item in items
      for n from 1
      for len = (length items)
      do 
      (setf (request-variable-value req var) item)
      (if status-var (setf (request-variable-value req status-var) (cons n len)))
      (emit-clp-entity req resp body))))

(defvar *case-tag* nil)

(def-clp-function clp_choose (req resp args body)
  (let ((*case-tag* (gensym)))
    (catch *case-tag*
           (emit-clp-entity req resp body))))

(def-clp-function clp_otherwise (req resp args body)
  (emit-clp-entity req resp body)
  (when *case-tag*
    (throw *case-tag* t)))

(def-clp-function clp_when (req resp args body)
  (when (eval-el-string req 
                        (arg-value "test" args) 
                        (arg-value "package" args "common-lisp"))
    (emit-clp-entity req resp body)
    (when *case-tag*
      (throw *case-tag* t))))

;;; CLP FORM ELEMENTS

(def-clp-function clp_input (req resp args body)
  (open-close-tag "input" (replace-value req args)))

(defvar *menu-selection* nil)

(def-clp-function clp_menu (req resp args body)
  (let ((new-args (replace-value req args)))
    (open-tag "select" new-args)
    (let ((*menu-selection* (attr-value "value" new-args)))
      (emit-clp-entity req resp body))
    (close-tag "select")))

(def-clp-function clp_option (req resp args body)
  (open-tag "option" (mark-if-selected args *menu-selection*))
  (emit-clp-entity req resp body)
  (close-tag "option"))

(defun mark-if-selected (args selected)
  (let ((value (attr-value "value" args)))
    (if (and value (equal value selected))
        (cons (cons "selected" "selected") args)
      args)))

(defun replace-value (req args)
  (let ((value-tail (member "value" args :key #'car :test #'equal)))
    (if (null value-tail) args
      (append (ldiff args value-tail)
              (acons (caar value-tail) 
                     (or (eval-el-string req (cdar value-tail) 
                                         (arg-value "package" args "common-lisp"))
                         "")
                     (cdr value-tail))))))

(defun open-tag (name args)
  (format *html-stream* "<~a" name)
  (dolist (arg args)
    (format *html-stream* " ~a=~s" (car arg) (cdr arg)))
  (format *html-stream* ">"))

(defun close-tag (name)
  (format *html-stream* "</~a>" name))

(defun open-close-tag (name args)
  (format *html-stream* "<~a" name)
  (dolist (arg args)
    (format *html-stream* " ~a=~s" (car arg) (cdr arg)))
  (format *html-stream* "/>"))

;;; UTILITY FUNCTIONS

(defun arg-value (key args &optional (default nil default-p))
  (let ((entry (assoc key args :test 'equal)))
    (or (cdr entry)
        (if default-p default (error "attribute ~S not found in ~S" key args)))))

(defun arg-values (key args)
  (cond ((null args) nil)
        ((equal (caar args) key)
         (cons (cdar args) (arg-values key (cdr args))))
        (t (arg-values key (cdr args)))))

(defun attr-entry (key attrs)
  (assoc key attrs :test #'equal))

(defun attr-value (key attrs)
  (cdr (attr-entry key attrs)))

(defun assoc-any (keys alist &key (test #'eql))
  (assoc-if #'(lambda (key) (member key keys :test test)) alist))

(defun eval-el-string (req str package)
  (eval-el req (read-el-string str package)))

(defun eval-el (req form)
  (eval (replace-vars req form)))
        
(defun el-package (package)
  (find-package (string-upcase package)))

(defun read-el-string (str package)
  (let ((*package* (el-package package)))
    (read-from-string str)))


(defun find-var-value (req name)
  (or (request-query-value name req)
      (request-variable-value req name)
      (websession-variable (websession-from-req req) name)))


(defun replace-vars (req form)
  (cond ((var-p form) (var-expansion req form))
        ((atom form) form)
        (t
         (let ((vals (mapcar #'(lambda (x) (replace-vars req x)) form)))
           (if (var-p (car form))
               `(lookup ,@vals)
             vals)))))


(defun var-p (form)
  (and (symbolp form) (eql (char (symbol-name form) 0) #\$)))

(defun var-name (form)
  (string-downcase (subseq (symbol-name form) 1)))

(defun var-expansion (req form)
  (list 'quote (find-var-value req (var-name form))))


;;; EXPERIMENTAL
;;; 
;;; Convert ($var key) to (lookup var-value key)
;;;
;;; The default lookup on a symbolic key does an assoc, skipping any initial atomic elements.
;;; To overload, (defmethod clp-exts:lookup ((x your-class) (key your-key-class)) ...)
;;;
;;; Ex: <clp_eval value="($person :name)" />

(defmethod lookup (form key &rest lst)
  (cond ((null form) nil)
        ((consp form) (lookup-alist form key))
        ((typep form 'simple-array) (lookup-array form key lst))
        ((standard-instance-p form) (lookup-slot form key))
        (t nil)))

(defun instance-package (instance)
  (symbol-package (class-name (class-of instance))))

(defun standard-instance-p (instance)
  (member (class-name (class-of (class-of instance)))
          '("STANDARD-CLASS" "STRUCTURE-CLASS")
          :test #'string=))

(defun lookup-alist (form key)
  (cdr (assoc key (member-if #'consp form)
              :test (if (symbolp key) #'eql #'equal))))

(defun lookup-array (form key lst)
  (apply #'aref form key lst))

(defun lookup-slot (form key)
  (slot-value form
              (find-symbol (symbol-name key)
                           (instance-package form))))
