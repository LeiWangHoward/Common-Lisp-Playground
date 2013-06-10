(defun line-to-list (file)
  (with-open-file (str file :direction :input)
    (do ((line (read-line str nil nil) (read-line str nil nil))
         (acc nil (cons line acc)))
        ((not line) (nreverse acc)))))

(defun s->list (file)
  (with-open-file (str file :direction :input)
    (do ((s (read str nil nil) (read str nil nil))
         (acc nil (cons s acc)))
        ((not s) (nreverse acc)))))
;;v 1.0 work on general strings
(defun map-stream (function stream)
  (with-input-from-string (str stream)
    (loop for x = (read str nil 'eof)
          until (eq x 'eof)
          collect x)))

;;v 2.0 work on actual 
(defun map-stream (function stream)
  (loop for x = (read stream nil 'eof)
        until (eq x 'eof)
        collect x))

;;  (coerce stream 'list))

(defun map-file
       (file &optional (out *standard-output*) (names (get-pattern-names)))
  (with-open-file (in file)
    (let ((eof (list nil)))
      (do ((code (read in nil eof) (read in nil eof)))
          ((eq code eof) (values))
        (print-separator out #\*)
        (let ((*print-right-margin* *output-width*))
          (pprint code out))
        (critique-definition code out names)))))