;;; A simple frame system
;;; ----------------------------------------------------------------------
;;; - File: mops.lisp
;;; - Author: Chris Riesbeck
;;; - Contributors: Will Fitzgerald, Matt Togliatti
;;;
;;; 11/17/10 added slot functions to the exports [CKR]
;;; 11/09/10 fixed redefinition bug with unlink-old-absts not updating
;;;   hierarchy [CKR]
;;; 12/02/09 updated SHOW-FRAME, SHOW-MEMORY for constraints [CKR]
;;; 12/02/09 exported ADD-FRAME, updated SHOW-FRAME, SHOW [CKR]
;;; 11/30/09 added constraint fillers to slots [CKR]
;;; 11/17/08 fixed and simplified find-instances so
;;;   that (find-instances 'animal '(:color gray))
;;;   returns (jumbo-1) etc. [CKR]
;;; 02/27/08 Fixed abst ordering bug in link-new-absts [CKR]
;;; 02/26/08 Merged mops.lisp, frames.lisp, show-frames.lisp [CKR]
;;; 06/06/06 Added :package parameter to add-frame [CKR]
;;; 03/04/04 Export all symbols used in a frame [CKR]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:mops
  (:use #:common-lisp)
  (:export #:defmop #:definstance #:mop-p #:instance-p
           #:add-frame #:add-instance #:find-instances 
           #:->frame #:root-frames #:clear-memory
           #:absts-of #:specs-of #:all-absts-of 
           #:inherit-filler #:role-filler #:<- #:abstp
           #:show-frame #:show-memory
           #:slots-of :#slot-role #:slot-filler)
  )


(in-package #:mops)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Globals and structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *frames* (make-hash-table))

(defstruct frame 
  name type slots absts specs all-absts)

(defstruct slot role filler constraint)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro defmop (name &optional absts &rest slots)
  `(add-frame ',name 
              :type :mop 
              :abstractions ',absts
              :slots ',slots))

(defmacro definstance (name &optional absts &rest slots)
  `(add-frame ',name 
              :type :instance
              :abstractions ',absts
              :slots ',slots))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Public functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mop-p (name &aux (frame (frame-of name)))
  (and frame (eql (frame-type frame) :mop)))

(defun instance-p (name &aux (frame (frame-of name)))
  (and frame (eql (frame-type frame) :instance)))

(defun find-instances (abst slots)
  (or (and (instance-p abst) 
           (has-slots-p abst slots)
           (list abst))
      (mapcan #'(lambda (spec)
                  (find-instances spec slots))
              (specs-of abst))))

(defun ->frame (name &key type)
  (or (gethash name *frames*)
      (setf (gethash name *frames*)
            (make-frame :name name :type type :all-absts (list name)))))

(defun clear-memory () (clrhash *frames*))

(defun root-frames ()
  (let ((roots nil))
    (maphash #'(lambda (key value)
                 (when (null (frame-absts value))
                   (push key roots)))
             *frames*)
    roots))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Private functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun frame-of (name) (gethash name *frames*))

(defun add-frame (name &key (package *package*) type abstractions slots)
  (export name package)
  (export-symbols-in abstractions package)
  (export-symbols-in slots package)
  (let ((frame (->frame name)))
    (setf (frame-type frame) type)
    (update-absts name
                  (if (listp abstractions)
                      abstractions
                      (list abstractions)))
    (setf (frame-slots frame) (collect-slots slots type))
    name))


(defun add-instance (abst slots &key (package *package*))
  (add-frame (make-instance-name abst :package package)
             :package package
             :type :instance
             :abstractions (list abst)
             :slots slots))

(defun make-instance-name (abst &key package)
  (gentemp (string abst) (or package *package*)))


(defun collect-slots (slots frame-type)
  (cond ((null slots) nil)
        (t (cons (make-frame-slot (car slots) (cadr slots) frame-type)
                 (collect-slots (cddr slots) frame-type)))))

(defun make-frame-slot (role filler frame-type)
  (ecase frame-type
    (:instance (make-slot :role role :filler filler))
    (:mop (apply #'make-slot :role role
                 (parse-slot filler)))))

(defun parse-slot (filler)
  (if (or (atom filler)
          (not (member (car filler) '(:filler :constraint))))
      (list :filler filler)
    filler))


;;; slots = (role filler role filler ...)
(defun has-slots-p (instance slots)
  (do ((slots slots (cddr slots)))
      ((or (null slots)
           (not (has-slot-p instance (car slots) (cadr slots) )))
       (null slots))))

(defun has-slot-p (instance role filler)
  (abstp filler
         (inherit-filler instance role)))

(defun export-symbols-in (l package)
  (cond ((and (symbolp l) (eql (symbol-package l) package))
         (export l))
        ((atom l) nil)
        (t (export-symbols-in (car l) package)
           (export-symbols-in (cdr l) package))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun slots-of (name &aux (frame (frame-of name)))
  (if frame (frame-slots frame)))
        
(defun absts-of (name &aux (frame (frame-of name)))
  (if frame (frame-absts frame)))
        
(defun specs-of (name &aux (frame (frame-of name)))
  (if frame (frame-specs frame)))

(defun all-absts-of (name &aux (frame (frame-of name)))
  (if frame (frame-all-absts frame) (list name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun abstp (abst spec)
 (or (eql abst spec)
     (not (null (member abst (all-absts-of spec))))))

(defun inherit (name fn)
  (some #'(lambda (abst) (funcall fn abst)) 
        (all-absts-of name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Abstraction hierarchy bookkeeping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (UPDATE-ABSTS name abstractions) => undefined
;;;    Changes name to have the given abstractions, and updates
;;;    memory accordingly.  This may involve removing old
;;;    abstraction links, and recalculating the all-absts field for
;;;    name and its children.

(defun update-absts (name absts)
  (let ((old-absts (absts-of name))
        (new-absts (remove-redundant-absts absts)))
    (unless (set-equalp old-absts new-absts)
      (unlink-old-absts name old-absts new-absts)
      (link-new-absts name old-absts new-absts))))

;;; (SET-EQUALP set1 set2) => true or false
;;;    Returns true if the two list have the same elements.  The
;;;    keyword arguments are passed to SUBSETP.

(defun set-equalp (set1 set2)  
  (and (subsetp set1 set2) (subsetp set2 set1)))

;;; (REMOVE-REDUNDANT-ABSTS absts) => absts
;;;    Returns absts, minus those items that are redundant.
;;; (REDUNDANT-ABST-P name absts) => true or false
;;;    returns true if name is redundant with respect to absts.
;;;
;;; An item is redundant with respect to a list if there is something
;;; strictly more specific than it in the list.

(defun remove-redundant-absts (absts)
  (remove-if #'(lambda (abst)
                 (redundant-abst-p abst absts))
             absts))

(defun redundant-abst-p (name absts)
  (and (not (null (specs-of name)))
       (member name absts :test #'strict-abstp)))

(defun strict-abstp (abst spec)
  (and (not (eql abst spec))
       (abstp abst spec)))

;;; (UNLINK-OLD-ABSTS name old-absts new-absts) => undefined
;;;   Unlinks name from any abstraction in old-absts that isn't
;;;   in new-absts.
;;; (LINK-NEW-ABSTS name old-absts new-absts) => undefined
;;;   Links name to any abstraction in new-absts that isn't
;;;   in old-absts.

(defun unlink-old-absts (name old-absts new-absts)
  (dolist (old-abst old-absts)
    (unless (member old-abst new-absts)
      (unlink-abst name old-abst)))
  (update-all-absts name))

;;; Because link-abst pushes, we reverse new-absts to preserve order
(defun link-new-absts (name old-absts new-absts)
  (dolist (new-abst (reverse new-absts))
    (unless (member new-abst old-absts)
      (link-abst name new-abst)))
  (update-all-absts name))

;;; (LINK-ABST spec abst) => undefined
;;;   Makes an immediate abstraction link from spec to abst.
;;;   If abst is already an abstraction of spec, nothing happens
;;;   If spec is an abstraction of abst, an error is signalled.
;;; (UNLINK-ABST spec abst) => undefined
;;;   Removes the immediate abstraction link from spec to abst,
;;;   if any.

(defun link-abst (spec abst)
  (cond ((abstp abst spec) nil)
        ((abstp spec abst)
         (error "~S can't be an abstraction of ~S" spec abst))
        (t
         (link-parent spec abst)
         (link-child spec abst))))

(defun unlink-abst (spec abst)
  (unlink-parent spec abst)
  (unlink-child spec abst))

;;; (LINK-PARENT spec abst) => undefined
;;;   Makes a parent link from spec to abst.
;;; (LINK-CHILD spec abst) => undefined
;;;   Makes a child link from abst to spec.

(defun link-parent (spec abst)
 (push abst (frame-absts (->frame spec))))

(defun link-child (spec abst)
  (push spec (frame-specs (->frame abst))))

;;; (UNLINK-PARENT spec abst) => undefined
;;;   Removes the parent link from spec to abst, if any.
;;; (UNLINK-CHILD spec abst) => undefined
;;;   Removes the child link from abst to spec, if any.

(defun unlink-parent (spec abst)
  (let ((spec-frame (frame-of spec)))
    (when spec-frame
      (setf (frame-absts spec-frame)
            (remove abst (frame-absts spec-frame))))))

(defun unlink-child (spec abst)
  (let ((abst-frame (frame-of abst)))
    (when abst-frame
      (setf (frame-specs abst-frame)
            (remove spec (frame-specs abst-frame))))))

;;; (UPDATE-ALL-ABSTS name) => undefined
;;;    Called when abstraction links are changed to recursively
;;;    fix the all-absts field for name and its children.

(defun update-all-absts (name)
  (let ((frame (->frame name)))
    (setf (frame-all-absts frame)
          (calc-all-absts name))
    (dolist (spec (frame-specs frame)) (update-all-absts spec))))

;;; (CALC-ALL-ABSTS ) => list of abstractions
;;;    Returns all the abstractions for name, in order.

(defun calc-all-absts (name)
  (cons name
        (remove-duplicates
         (mapcan #'(lambda (abst)
                     (copy-list (all-absts-of abst)))
                 (absts-of name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun role-slot (name role) 
  (find role (slots-of name) :key #'slot-role))
    
(defun role-filler (name role) 
  (let ((slot (role-slot name role)))
    (and slot (slot-filler slot))))

(defun inherit-filler (name role)
  (inherit name #'(lambda (abst) (role-filler abst role))))
        
(defun <- (name &rest roles)
  (do ((roles roles (rest roles))
       (name name (inherit-filler name (first roles))))
      ((or (endp roles) (null name)) name)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SHOW-MEMORY and SHOW-FRAME
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; (SHOW-MEMORY [concept stream slots-p])
;;;    Prints the tree of concepts under concept. If none is given,
;;;    all trees under all root concepts are printed.
;;;    Stream defaults to standard output. 
;;;    If slots-p is true, slots for each concept are printed.
;;;    A concept's subtree and slots are printed once, even if the
;;;    concept has multiple parents.
;;; (SHOW-FRAME concept)
;;;    Pretty-prints the slots of a concept, the slots of the fillers
;;;    and so on.
;;;
;;; Exs.
;;;   > (use-package :frames)
;;;   > (show-memory)
;;;   ... prints the tree of concepts under all "root" nodes
;;;   > (show-memory nil t t)
;;;   ... prints all concepts and their slots
;;;   > (show-frame 'i-cheetah-chase-antelope)
;;;   ... prints the slots of I-CHEETAH-CHASE-ANTELOPE and of its fillers


(defun show-memory (&optional name
                    (stream *standard-output*) slots-p
                    &aux shown)
  (labels ((show (name prefix)
             (let ((specs (specs-of name))
                   (slots (and slots-p (slots-of name))))
               (cond ((member name shown)
                      (format stream
                              (if (or specs slots) "~S...~%" "~S~%") name))
                 (t
                  (format stream "~S~%" name)
                  (push name shown)
                  (when slots
                    (let ((bar (if specs "|" " ")))
                      (dolist (slot slots)
                         (if (slot-p slot)
                             (format stream "~A ~A ~S~@[ ~S~]~@[ :CONSTRAINT ~S~]~%" prefix bar
                               (slot-role slot)
                               (slot-filler slot)
                               (slot-constraint slot))
                             (format stream "~A ~A ~S are:~%"
                                     prefix bar slot)))))
                  (when specs
                    (do ((next-prefix (format nil "~A |   " prefix))
                         (last-prefix (format nil "~A     " prefix))
                         (l specs (cdr l)))
                        ((null (cdr l))
                         (format stream "~A +-- " prefix)
                         (show (car l) last-prefix))
                        (format stream "~A |-- " prefix)
                      (show (car l) next-prefix))))))))
    (if (null name)
        (dolist (name (root-frames))
          (show name ""))
      (show name ""))
    (values)))

;;; Printing internal frame structures
;;; ----------------------------------------------------------------------


;;; (SHOW-FRAME name) => no values
;;;   Prints the internals of the frame named in a readable fashion.
;;;
;;; SHOW-FRAME recursively prints the internal structure of all parts
;;; of the frame, if they haven't already been printed.

(defvar *frames-shown* nil)

(defun show-frame (name)
  (let ((*frames-shown* '()))
    (pprint-frame-info name 4)
    (values)))

;;; (PPRINT-FRAME-INFO name left-margin) => undefined

;;; PPRINT-FRAME-INFO prints internal frame structures in a readable
;;; fashion, indented left-margin number of spaces.

(defun pprint-frame-info (name left-margin)
  (unless (member name *frames-shown*)
    (push name *frames-shown*)
    (let ((frame (frame-of name)))
      (unless (null frame)
        (pprint-frame-type frame left-margin)
        (pprint-frame-absts frame left-margin)
        (pprint-frame-slots frame left-margin)))))

(defun pprint-frame-type (frame left-margin)
  (let ((type (frame-type frame)))
    (when type
      (format t "~&~VTTYPE ~S~%" left-margin type))))

(defun pprint-frame-absts (frame left-margin)
  (loop for abst in (frame-absts frame)
        do (format t "~&~VTISA ~S~%" left-margin abst)))

(defun pprint-frame-slots (frame left-margin)
  (loop for slot in (frame-slots frame)
        do (format t "~&~VT~S~@[ ~S~]~@[ :CONSTRAINT ~S~]~%" 
             left-margin 
             (slot-role slot)
             (slot-filler slot)
             (slot-constraint slot))
        (pprint-frame-info (slot-filler slot)
                           (+ left-margin 2))))

  
(provide "mops")
