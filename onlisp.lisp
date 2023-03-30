(defun my-remove-if (pred lst)
  "Not include the element if the predicate is true"
  (if (null lst)
      nil
      (if (funcall pred (car lst))
	  (my-remove-if pred (cdr lst))
	  (cons (car lst) (my-remove-if pred (cdr lst))))))

;; usage
;; (my-remove-if #'evenp '(1 2 3 3 4 4 5 5 6 6 7 7))
;; -> (1 3 3 5 5 7 7)

;; multiple predicates
(my-remove-if #'(lambda (x)
		  (or (> 7 x) (evenp x)))
	      (list 1 2 3 4 5 6 7 8 9 10 11 12 13))

(defun list+ (lst n)
  (mapcar #'(lambda (x)
	      (+ x n))
	  lst))

(defun make-adder (n)
  #'(lambda (x &optional reset)
      (if reset
	  (setq n x)
	  (+ x n))))

;; (setq addx (make-adder 3))
;; (funcall addx 1)
;; -> 4
;; (funcall addx 100 t)
;; -> 100
;; (funcall addx 1)
;; -> 101

(defun make-dbms (db)
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(defun count-instances (obj lsts)
  "Count the presence of `obj' inside of `lsts'."
  (labels ((instance (lst)
	     (if (consp lst)
		 (+ (if (eq (car lst) obj) 1 0)
		    (instance (cdr lst))))))
    (mapcar #'instance lsts)))

(defun my-reverse-lst (lst)
  "Tail reverse function for list."
  (labels ((rev (lst acc)
	     (if (null lst)
		 acc
		 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))
