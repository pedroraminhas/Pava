;Here are the aux functions called by the monadic functions
(defun .-Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (symmetric it)))))))

(defun ./Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (inverse it)))))))

(defun .!Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (factorial it)))))))

(defun .sinAux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (sin it)))))))

(defun .cosAux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (cos it)))))))

(defun .notAux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (negation it)))))))



;MONADIC FUNCTIONS
(defun .- (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.-Aux x))
		  (t (make-instance 'tensor :tensor-elements (symmetric x-elements) :tensor-has "s")))))


(defun ./ (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(./Aux x))
		  (t (make-instance 'tensor :tensor-elements (inverse x-elements) :tensor-has "s")))))


(defun .! (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.!Aux x))
		  (t (make-instance 'tensor :tensor-elements (factorial x-elements) :tensor-has "s")))))

(defun .sin (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.sinAux x))
		  (t (make-instance 'tensor :tensor-elements (sin x-elements) :tensor-has "s")))))

(defun .cos (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.cosAux x))
		  (t (make-instance 'tensor :tensor-elements (cos x-elements) :tensor-has "s")))))

(defun .not (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.notAux x))
		  (t (make-instance 'tensor :tensor-elements (negation x-elements) :tensor-has "s")))))



;Returns tensor with elements from 1 to x
(defun interval (x)
	(let ((value ()))
		(dotimes (it x (make-instance 'tensor :tensor-elements value :tensor-has "v"))
			(setf value  (append value (list (+ it 1)))))))	

;Shape function
(defun shape (tensor)
	(let ((tensor-elements (slot-value tensor 'elements)))
		(cond 	 ((not (listp tensor-elements)) 1)
				 ((not  (listp (car tensor-elements))) (print (length tensor-elements))) 
				 (t	(progn 
				 		(shape (make-instance 'tensor :tensor-elements (car tensor-elements) :tensor-has "v")))
				 		(shape (make-instance 'tensor :tensor-elements (cdr tensor-elements) :tensor-has "v"))))))