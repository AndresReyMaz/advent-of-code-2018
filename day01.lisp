(defun part2 (lst)
  (let ((idx 0)
        (h (make-hash-table))
        (cur 0))
    (setf (gethash cur h) t)
    (loop (setf cur (+ cur (nth idx lst)))
       (incf idx)
       (if (>= idx (length lst)) (setf idx 0))
       (if (gethash cur h) (return cur))
       (setf (gethash cur h) t))))

(defun part1 (lst)
  (reduce #'+ lst))

(defun read-to-list (n in)
  (if (> n 0) (cons (parse-integer (read-line in)) (read-to-list (decf n) in)) ()))


(defun main ()
  (let ((in (open "./1.txt")))
    (let ((lst (read-to-list 1028 in)))
      (print (part1 lst))
      (print (part2 lst))
    (close in))))
