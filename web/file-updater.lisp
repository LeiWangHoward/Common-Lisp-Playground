;;V 1.0 Not Mine

(ql:quickload "net-telent-date")

(defun update-file (filename path url)
  (let ((filepath-local (merge-pathnames path filename))
        (filepath-online (concatenate 'string url filename)))
    (cond ((or (not (probe-file filepath-local))
               (> (date:parse-time (get-url-modified-time filepath-online))
                  (file-write-date filepath-local)))
           (if (yes-or-no-p "New version available. Update now?")
               (copy-remote-to-local filepath-local filepath-online)
               (format t "Bye!")))
          (t (format t "The file is updated!")))))

(defun get-url-modified-time (url)
  (multiple-value-bind (content response head)
      (net.aserve.client:do-http-request url :method :head)
    (cdr (assoc :last-modified head))))

(defun copy-remote-to-local (filename url)
  (with-open-file (out filename :direction :output :if-exists :supersede)
    (write-string (net.aserve.client:do-http-request url :method :get) out)))

;; v 2.0
;;client-request-headers
#|(update-file "exercise-tests.lisp"
               "/Users/leiwang/Documents/courses/cs325/"
               "http://www.cs.northwestern.edu/academics/courses/325/programs/")|#
;;http://www.cs.northwestern.edu/academics/courses/325/programs/basic-demo.zip
(ql:quickload "net-telent-date")

(defun update-file (filename path url)
  (let ((local-path (merge-pathnames path filename))
        (online-path (concatenate 'string url filename)))
    (cond ((null (probe-file local-path))       ;file not exist, ask user first
           (if (yes-or-no-p "File does not exist locally. Download now?")
               (update-local local-path online-path)
             (format t "File not downloaded.")))
          ((> (url-modified-date online-path) (file-write-date local-path))   ;;newer version exist, update
           ((update-local local-path online-path)
            (format t "File updated")))
          (t (format t "The file is up-to-date!")))))

(defun url-modified-date (url)
  (multiple-value-bind (empty-content response head)
      (net.aserve.client:do-http-request url :method :head)
    (date:parse-time (cdr (assoc :last-modified head)))))

(defun update-local (local-path url)
  (with-open-file (buffer local-path :direction :output :if-exists :supersede)
    (write-string (net.aserve.client:do-http-request url :method :get) buffer)))
