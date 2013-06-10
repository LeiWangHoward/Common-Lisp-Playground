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