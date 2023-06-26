(defun quit-char (str char)
  "Remove the char `char' from the string `str'."
  (remove-if #'(lambda (c)
		 (equal c char))
	     str))

(defun replace-char (lst-of-chars char char-replace)
  "Replace the char `char' for the list of characters `lst-of-chars'."
  (mapcar #'(lambda (c)
	      (if (equal c char)
		  char-replace
		  c))
	  lst-of-chars))

(defun take (num lst)
  "Return a list of `num' length elements, from the list `lst'."
  (cond ((null lst) nil)
	((zerop num) nil)
	(t (cons (car lst)
		 (take (1- num) (cdr lst))))))

(defun range (start stop &optional (step 1))
  "Return a list from the `start' value to the `stop' value, jumping using
the `step', the default value for `step' is 1."
  (if (>= start stop)
      nil
      (cons start
	    (range (+ start step) stop step))))

(defun out-last (start stop lst)
  "Return the list except with the last element."
  (if (>= start stop)
      nil
      (cons (car lst)
	    (out-last (1+ start) stop (cdr lst)))))

(defun not-last (lst)
  "Wrapper over `out-last' using only the list `lst' as parameter."
  (out-last 0 (1- (length lst)) lst))

(defun flatten (lst)
  "Flat a list of lists."
  (labels ((flat (elem acc)
	     (cond ((equal elem nil) acc)
		   ((atom elem) (cons elem acc))
		   (t (flat (car elem) (flat (cdr elem) acc))))))
    (flat lst nil)))
