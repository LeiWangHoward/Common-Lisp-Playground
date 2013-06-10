;;v 1.0 not correct amount of lines
(defun assert-file (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil '#\) )
               (read-line str nil '#\) ))
         (count 0 (1+ count)))
        ((eql line '#\)) count))))

;;v 2.0 good, need add tell
(defun assert-file (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil)
               (read-line str nil))
         (count 0 (1+ count)))
        ((null line) count)
      (cond ((find #\) line :test #'equal)
             (print (string-trim '(#\Space #\( #\)) line)))
            (t 
             (print (string-trim '(#\Space #\( #\)) (concatenate 'string line (read-line str nil)))))))))
     
(defun trim-to-list (stream)
  ;(string-trim '(#\Space #\( #\)) stream)
  (with-input-from-string 
      (str (string-trim '(#\Space #\( #\)) stream))
    (loop for x = (read str nil 'eof)
          until (eq x 'eof)
          collect x)))

;;v3.0 great!
(defun assert-file (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil)
               (read-line str nil))
         (count 0 (1+ count)))
        ((null line) count)
      (cond ((find #\) line :test #'equal)
             (tell (trim-to-list line)))
            (t 
             (tell (trim-to-list
              (concatenate 'string line (read-line str nil)))))))))
     
(defun trim-to-list (stream)
  (with-input-from-string 
      (str (string-trim '(#\Space #\( #\)) stream))
    (loop for x = (read str nil 'eof)
          until (eq x 'eof)
          collect x)))

