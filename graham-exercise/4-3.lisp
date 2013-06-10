(defstruct 3tree data left middle right)

(defun 3tree-clone (3t)
  (when 3t
      (make-3tree :data (3tree-data 3t)
                  :left (3tree-clone (3tree-left 3t))
                  :middle (3tree-clone (3tree-middle 3t))
                  :right (3tree-clone (3tree-right 3t)))))

(defun 3tree-member (obj 3t)
  (when 3t
      (or
       (eql obj (3tree-data 3t))
       (3tree-member obj (3tree-left 3t))
       (3tree-member obj (3tree-middle 3t))
       (3tree-member obj (3tree-right 3t)))))