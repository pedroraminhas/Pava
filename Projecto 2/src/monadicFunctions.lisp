;MONADIC FUNCTIONS

;Symmetric
(defgeneric .- (x))

(defmethod .- ((x scalar))
	(let ((x-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (symmetric x-elements))))

(defmethod .- ((x vector-tensor))
	(let ((list-elements (slot-value x 'elements))
 		 (list-result ()))
 			(dolist (it list-elements (make-instance 'vector-tensor :tensor-elements list-result))
	 				(setf list-result (append list-result (list (symmetric it)))))))

(defmethod .- ((x matrix))
	(let ((matrix-elements (slot-value x 'elements))
		 (matrix-result ())
		 (vector-resut ()))
		 	(dolist (it matrix-elements (make-instance 'matrix :tensor-elements matrix-result))

		 		(progn
			 		(dolist (it2 it)
					  	(setf vector-resut (append vector-resut (list (symmetric it2)))))

			 		(setf matrix-result (append matrix-result (list vector-resut)))
			 		(setf vector-resut ())))))


;Inverse
(defgeneric ./ (x))

(defmethod ./ ((x scalar))
	(let ((x-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (inverse x-elements))))

(defmethod ./ ((x vector-tensor))
	(let ((list-elements (slot-value x 'elements))
 		 (list-result ()))
 			(dolist (it list-elements (make-instance 'vector-tensor :tensor-elements list-result))
 				(setf list-result (append list-result (list (inverse it)))))))

(defmethod ./ ((x matrix))
	(let ((matrix-elements (slot-value x 'elements))
		 (matrix-result ())
		 (vector-resut ()))
		 	(dolist (it matrix-elements (make-instance 'matrix :tensor-elements matrix-result))

		 		(progn
			 		(dolist (it2 it)
					  	(setf vector-resut (append vector-resut (list (inverse it2)))))

			 		(setf matrix-result (append matrix-result (list vector-resut)))
			 		(setf vector-resut ())))))


;Factorial
(defgeneric .! (x))

(defmethod .! ((x scalar))
	(let ((x-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (factorial x-elements))))

(defmethod .! ((x vector-tensor))
	(let ((list-elements (slot-value x 'elements))
 		 (list-result ()))
 			(dolist (it list-elements (make-instance 'vector-tensor :tensor-elements list-result))
 				(setf list-result (append list-result (list (factorial it)))))))

(defmethod .! ((x matrix))
	(let ((matrix-elements (slot-value x 'elements))
		 (matrix-result ())
		 (vector-resut ()))
		 	(dolist (it matrix-elements (make-instance 'matrix :tensor-elements matrix-result))

		 		(progn
			 		(dolist (it2 it)
					  	(setf vector-resut (append vector-resut (list (factorial it2)))))

			 		(setf matrix-result (append matrix-result (list vector-resut)))
			 		(setf vector-resut ())))))
;Sin
(defgeneric .sin (x))

(defmethod .sin ((x scalar))
	(let ((x-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (sin x-elements))))

(defmethod .sin ((x vector-tensor))
	(let ((list-elements (slot-value x 'elements))
 		 (list-result ()))
 			(dolist (it list-elements (make-instance 'vector-tensor :tensor-elements list-result))
 				(setf list-result (append list-result (list (sin it)))))))

(defmethod .sin ((x matrix))
	(let ((matrix-elements (slot-value x 'elements))
		 (matrix-result ())
		 (vector-resut ()))
		 	(dolist (it matrix-elements (make-instance 'matrix :tensor-elements matrix-result))

		 		(progn
			 		(dolist (it2 it)
					  	(setf vector-resut (append vector-resut (list (sin it2)))))

			 		(setf matrix-result (append matrix-result (list vector-resut)))
			 		(setf vector-resut ())))))

;Cos
(defgeneric .cos (x))

(defmethod .cos ((x scalar))
	(let ((x-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (cos x-elements))))

(defmethod .cos ((x vector-tensor))
	(let ((list-elements (slot-value x 'elements))
 		 (list-result ()))
 			(dolist (it list-elements (make-instance 'vector-tensor :tensor-elements list-result))
 				(setf list-result (append list-result (list (cos it)))))))

(defmethod .cos ((x matrix))
	(let ((matrix-elements (slot-value x 'elements))
		 (matrix-result ())
		 (vector-resut ()))
		 	(dolist (it matrix-elements (make-instance 'matrix :tensor-elements matrix-result))

		 		(progn
			 		(dolist (it2 it)
					  	(setf vector-resut (append vector-resut (list (cos it2)))))

			 		(setf matrix-result (append matrix-result (list vector-resut)))
			 		(setf vector-resut ())))))

;Negation
(defgeneric .not (x))

(defmethod .not ((x scalar))
	(let ((x-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (negation x-elements))))

(defmethod .not ((x vector-tensor))
	(let ((list-elements (slot-value x 'elements))
 		 (list-result ()))
 			(dolist (it list-elements (make-instance 'vector-tensor :tensor-elements list-result))
 				(setf list-result (append list-result (list (negation it)))))))

(defmethod .not ((x matrix))
	(let ((matrix-elements (slot-value x 'elements))
		 (matrix-result ())
		 (vector-resut ()))
		 	(dolist (it matrix-elements (make-instance 'matrix :tensor-elements matrix-result))

		 		(progn
			 		(dolist (it2 it)
					  	(setf vector-resut (append vector-resut (list (negation it2)))))

			 		(setf matrix-result (append matrix-result (list vector-resut)))
			 		(setf vector-resut ())))))




;Returns tensor with elements from 1 to x
(defun interval (x)
	(let ((value ()))
		(dotimes (it x (make-instance 'vector-tensor :tensor-elements value))
			(setf value  (append value (list (+ it 1)))))))	

;Shape function
(defgeneric shape (x))

(defmethod shape ((x scalar))
	1)

(defmethod shape ((x vector-tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(length tensor-elements)))




; (defun shape (tensor)
; 	(let ((tensor-elements (slot-value tensor 'elements)))
; 		(cond 	 ((not (listp tensor-elements)) 1)
; 				 ((not  (listp (car tensor-elements))) (print (length tensor-elements))) 
; 				 (t	(progn 
; 				 		(shape (make-instance 'tensor :tensor-elements (car tensor-elements) :tensor-has "v")))
; 				 		(shape (make-instance 'tensor :tensor-elements (cdr tensor-elements) :tensor-has "v"))))))