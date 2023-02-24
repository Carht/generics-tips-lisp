;; A simple tail recursive factorial

(defun fact (n)
  (labels ((factorial (x acc)
	     (if (zerop x)
		 acc
		 (factorial (- x 1) (* x acc)))))
    (factorial x 1)))

(fact 3456)
