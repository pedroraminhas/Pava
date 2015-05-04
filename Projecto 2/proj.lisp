;To load the file use (load "proj.lisp")

(DEFGENERIC s (x))

(defmethod s ((x number))
	x)

(defun size-of-args (args)
	(length args))

 (defun v (&rest y)
 	(format t "~{~a~^ ~}" y)(values))


 ; (defun v (&rest y)
 ; 	  (dotimes (i (size-of-args y))

 ; 	  	  (princ (s (nth i y)))
	  	
 ; 	  	  t))
	  

