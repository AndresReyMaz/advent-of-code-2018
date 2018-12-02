(defun int-to-char (n)
  (code-char (+ n 97)))

(defun has-exactly-n (n str)
  (dotimes (i 26)
    (if (eql n (count (int-to-char i) str)) (return-from has-exactly-n t)))
  nil)

(defun vector-to-list (v)
  (map 'list #'identity v))

(defun differ-by-1 (lst1 lst2)
  "Checks whether two equal-length character lists are different by exactly one element"
  (if (null lst1) nil
      (if (char/= (first lst1) (first lst2))
          (equal (rest lst1) (rest lst2))
          (differ-by-1 (rest lst1) (rest lst2)))))

(defun part1 (lst)
  (* (count-if #'(lambda (x) (has-exactly-n 2 x)) lst) (count-if #'(lambda (x) (has-exactly-n 3 x)) lst)))

(defun part2 (lst)
  (dotimes (i (length lst))
    (dotimes (j (length lst))
      (if (and (/= i j) (differ-by-1 (vector-to-list (nth i lst)) (vector-to-list (nth j lst))))
          (progn
            (print (nth i lst))
            (print (nth j lst)))))))

(defun read-to-list (n in)
  (if (> n 0) (cons (read-line in) (read-to-list (decf n) in)) ()))


(defun main ()
  (let ((in (open "./2.txt")))
    (let ((lst (read-to-list 250 in)))
      (print (part1 lst))
      (print (part2 lst))
    (close in))))

