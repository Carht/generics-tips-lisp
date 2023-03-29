(defun my-length (lst)
  "Non tail recursive function."
  (if (null lst)
      0
      (1+ (my-length (cdr lst)))))

(defun my-tail-length (lst)
  "Tail recursive function, using an internal function."
  (labels ((rec (lst acc)
	     (if (null lst)
		 acc
		 (rec (cdr lst) (1+ acc)))))
    (rec lst 0)))
