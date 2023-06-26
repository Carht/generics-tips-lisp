(defun file-to-lst-fn (file-path fn)
  "The file `file-path' is transformed to a list of chars, the function `fn' apply
some procedure before return the list of characters in a string, 
each string is a file line."
  (with-open-file (in file-path)
    (loop for line = (read-line in nil nil)
	  while line
	  collect (funcall fn (coerce line 'list)))))
