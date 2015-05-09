;Returns the symmetric of a number
(defun symmetric (x)
	(- 0 x))

;Returns the inverse of a number
(defun inverse (x)
	(/ 1 x))

;Returns the factorial of a number
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1))) ) )

;Returns the negation of a number
(defun negation (x)
	(cond ((eq x 0) 1)
			(t 0)))