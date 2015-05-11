;Dyadic functions

;Subtraction
(defun .- (x &optional y)
	(if (eq y NIL)	(.-monadic x) (.-dyadic x y)))

(defgeneric .-dyadic (x y))

(defmethod .-dyadic ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (subtraction-tensor-scalar x y))
	)

(defmethod .-dyadic ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (subtraction-tensor-scalar x y))
	)

(defmethod .-dyadic ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (subtraction-tensor-scalar y x))
	)

(defmethod .-dyadic ( (y scalar) (x tensor))
			(make-instance 'vector-tensor :tensor-elements (subtraction-tensor-scalar y x))
	)

(defgeneric subtraction-tensor-scalar (t1 t2))

(defmethod subtraction-tensor-scalar ((t1 tensor) (t2 scalar))
	(let ((result ())
		 (tensor-elements (slot-value t1 'elements)))
		(cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (- it (slot-value t2 'elements)))))))
				(t (loop for x in tensor-elements
					do (setf result (append result (list (subtraction-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
		result))

(defmethod subtraction-tensor-scalar ((t2 scalar) (t1 tensor))
	(let ((result ())
		 (tensor-elements (slot-value t1 'elements)))
		(cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (- (slot-value t2 'elements) it))))))
				(t (loop for x in tensor-elements
					do (setf result (append result (list (subtraction-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
		result))

(defmethod .-dyadic ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (eq t1-shape t2-shape) (subtraction-tensor-tensor t1-elements t2-elements) (error "Tensors dont have the same size"))))



(defun subtraction-tensor-tensor (t1 t2)
	(let ((result ()))
		(cond ((not (listp (car t1))) (loop for x in t1
											for y in t2
										 (setf result (append result (list (- x y))))))
				(t (loop for x in t1
						 for y in t2
					do (setf result (append result (list (subtraction-tensor-tensor t1 t2)))))))
		result))
	





