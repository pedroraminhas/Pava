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

(defmethod .- ((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'tensor :tensor-elements (.-Aux tensor-elements))))

(defun .-Aux (tensor-elements)
	(let ((result ()))
		(cond ((not (listp (car (car tensor-elements)))) (slot-value (.- (make-instance 'matrix :tensor-elements tensor-elements)) 'elements))
				( t (progn (loop for x in tensor-elements
						  			do (progn (setf result (append result (list (.-Aux x))))))
									result)))))



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

(defmethod ./ ((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'tensor :tensor-elements (./Aux tensor-elements))))

(defun ./Aux (tensor-elements)
	(let ((result ()))
		(cond ((not (listp (car (car tensor-elements)))) (slot-value (./ (make-instance 'matrix :tensor-elements tensor-elements)) 'elements))
				( t (progn (loop for x in tensor-elements
						  			do (progn (setf result (append result (list (./Aux x))))))
									result)))))


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

(defmethod .! ((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'tensor :tensor-elements (.!Aux tensor-elements))))

(defun .!Aux (tensor-elements)
	(let ((result ()))
		(cond ((not (listp (car (car tensor-elements)))) (slot-value (.! (make-instance 'matrix :tensor-elements tensor-elements)) 'elements))
				( t (progn (loop for x in tensor-elements
						  			do (progn (setf result (append result (list (.!Aux x))))))
									result)))))
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

(defmethod .sin ((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'tensor :tensor-elements (.sinAux tensor-elements))))

(defun .sinAux (tensor-elements)
	(let ((result ()))
		(cond ((not (listp (car (car tensor-elements)))) (slot-value (.sin (make-instance 'matrix :tensor-elements tensor-elements)) 'elements))
				( t (progn (loop for x in tensor-elements
						  			do (progn (setf result (append result (list (.sinAux x))))))
									result)))))

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

(defmethod .cos ((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'tensor :tensor-elements (.cosAux tensor-elements))))

(defun .cosAux (tensor-elements)
	(let ((result ()))
		(cond ((not (listp (car (car tensor-elements)))) (slot-value (.cos (make-instance 'matrix :tensor-elements tensor-elements)) 'elements))
				( t (progn (loop for x in tensor-elements
						  			do (progn (setf result (append result (list (.cosAux x))))))
									result)))))
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


(defmethod .not((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'tensor :tensor-elements (.notAux tensor-elements))))

(defun .notAux (tensor-elements)
	(let ((result ()))
		(cond ((not (listp (car (car tensor-elements)))) (slot-value (.not (make-instance 'matrix :tensor-elements tensor-elements)) 'elements))
				( t (progn (loop for x in tensor-elements
						  			do (progn (setf result (append result (list (.notAux x))))))
									result)))))

;Returns tensor with elements from 1 to x
(defun interval (x)
	(let ((value ()))
		(dotimes (it x (make-instance 'vector-tensor :tensor-elements value))
			(setf value  (append value (list (+ it 1)))))))	

;Shape function
(defgeneric shape (x))

(defmethod shape ((x scalar))
	(v 0)) ; porque o shape de um escalar é zero

(defmethod shape ((x vector-tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'scalar :tensor-elements (length tensor-elements))))

;Faz o shape qualquer tensor que nao seja escalar ou vector
(defmethod shape ((x tensor))
	(let ((tensor-elements (slot-value x 'elements)))
		(make-instance 'vector-tensor :tensor-elements (tensor-dims tensor-elements))))

(defun tensor-dims (tensor)
  (if (not (listp tensor))
      ()
      (cons (length tensor) (tensor-dims (car tensor)))))


