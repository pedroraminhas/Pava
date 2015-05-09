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




;Returns tensor with elements from 1 to x
(defun interval (x)
	(let ((value ()))
		(dotimes (it x (make-instance 'vector-tensor :tensor-elements value))
			(setf value  (append value (list (+ it 1)))))))	

;Shape function
(defun shape (tensor)
	(let ((tensor-elements (slot-value tensor 'elements)))
		(cond 	 ((not (listp tensor-elements)) 1)
				 ((not  (listp (car tensor-elements))) (print (length tensor-elements))) 
				 (t	(progn 
				 		(shape (make-instance 'tensor :tensor-elements (car tensor-elements) :tensor-has "v")))
				 		(shape (make-instance 'tensor :tensor-elements (cdr tensor-elements) :tensor-has "v"))))))