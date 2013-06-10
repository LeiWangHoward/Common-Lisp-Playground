#|(defun camelize (string &optional capitalize)
  (with-input-from-string (istream string)
    (do ((c (read-char istream) (read-char istream nil 'the-end)))
        ((not (characterp c)) string)
      (when (eql c #\-)
        (setf c #\s))))
  ; (substitute-if #\- #'upper-case-p string)
   (if (eql capitalize t)
       (string-capitalize string))
   string)|#

#| ;;Version1.0
(defun camelize (string &optional capitalize)
  (do ((i 0 (1+ i)))
       ((= i (length string)) t)
      (when (eql (char string i) #\-)
        (incf i)
        (setf (char string i) (char-upcase (char string i)))))
      (setf string (remove #\- string))
  ; (substitute-if #\- #'upper-case-p string)
   (unless (null capitalize)
       (setf (char string 0) (char-upcase (char string 0))))
   string)
       ;(string-capitalize string)
|#
;;version 2.0
(defun camelize (string &optional capitalize)
  (let ((i 0))
    (when (< i (length string))
      (when (eql (char string i) #\-)
        (incf i)
        (setf (char string i) (char-upcase (char string i))))
      (incf i))
    (unless (null capitalize)
      (setf (char string 0) (char-upcase (char string 0)))))
  (remove #\- string))

;;version 1.0 TODO:make it work!
(defun hyphenate (string)
  (with-output-to-string (stream)
    (let ((i 0)
          (lowercase-before nil))      
    (when (< i (length string))
      (cond ((and lowercase-before  (upper-case-p (char string i)))
             (princ #\- stream))
            ((lower-case-p (char string i)) (setf lowercase-before t))
            ((lower-case-p (char string i)) (setf lowercase-before nil)))
      (princ (char string i) stream)
      (incf i)))
    stream))

;;version 2.0 loop is so cool
(defun hyphenate (camel-string &optional (case :upper))
  (with-output-to-string (result)
     (loop for c across camel-string
           with lower-case-before
           when (and lower-case-before
                     (upper-case-p c))
             do (princ "-" result)
           if (lower-case-p c)
             do (setf lower-case-before t)
           else
             do (setf lower-case-before nil)
           if (eql case :upper)  
             do (princ (char-upcase c) result)
           else 
             do (princ (char-downcase c) result))))
    
#|
(string-upcase "Cool")
"COOL"
(string-downcase "COOL")
"cool"
(string-downcase "Cool")
"cool"
(string-capitalize "cool")
"Cool"
|#