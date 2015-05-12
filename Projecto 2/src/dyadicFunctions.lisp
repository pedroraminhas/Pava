;Dyadic functions

;Subtraction
(defun .- (x &optional y)
	(if (eq y NIL)	(.-monadic x) (.-dyadic x y)))

(defgeneric .-dyadic (x y))

(defmethod .-dyadic ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (- x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .-dyadic ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (subtraction-tensor-scalar x y))
	)

(defmethod .-dyadic ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (subtraction-tensor-scalar x y))
	)

(defmethod .-dyadic ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (subtraction-tensor-scalar y x))
	)

(defmethod .-dyadic ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (subtraction-tensor-scalar y x))
	)


(defmethod .-dyadic ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (subtraction-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .-dyadic ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (subtraction-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

;Addition
(defgeneric .+ (x y))

(defmethod .+ ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (+ x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .+ ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (addition-tensor-scalar x y))
	)

(defmethod .+ ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (addition-tensor-scalar x y))
	)

(defmethod .+ ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (addition-tensor-scalar y x))
	)

(defmethod .+ ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (addition-tensor-scalar y x))
	)


(defmethod .+ ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (addition-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .+ ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (addition-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))
	
;Multiplication
(defgeneric .* (x y))

(defmethod .* ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (* x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .* ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (multiplication-tensor-scalar x y))
	)

(defmethod .* ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (multiplication-tensor-scalar x y))
	)

(defmethod .* ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (multiplication-tensor-scalar y x))
	)

(defmethod .* ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (multiplication-tensor-scalar y x))
	)


(defmethod .* ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (multiplication-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .* ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (multiplication-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))


;Division

(defun ./ (x &optional y)
	(if (eq y NIL)	(./monadic x) (./dyadic x y)))

(defmethod ./dyadic ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (/ x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod ./dyadic ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (division-tensor-scalar x y))
	)

(defmethod ./dyadic ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (division-tensor-scalar x y))
	)

(defmethod ./dyadic ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (division-tensor-scalar y x))
	)

(defmethod ./dyadic ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (division-tensor-scalar y x))
	)


(defmethod ./dyadic ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (division-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod ./dyadic ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (division-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

;Integer Division

(defgeneric .// (x y))

(defmethod .// ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (values (floor x-elements y-elements))))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .// ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (integer-division-tensor-scalar x y))
	)

(defmethod .// ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (integer-division-tensor-scalar x y))
	)

(defmethod .// ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (integer-division-tensor-scalar y x))
	)

(defmethod .// ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (integer-division-tensor-scalar y x))
	)


(defmethod .// ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (integer-division-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .// ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (integer-division-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

;Remainder of integer division

(defgeneric .% (x y))

(defmethod .% ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (mod x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .% ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (mod-tensor-scalar x y))
	)

(defmethod .% ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (mod-tensor-scalar x y))
	)

(defmethod .% ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (mod-tensor-scalar y x))
	)

(defmethod .% ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (mod-tensor-scalar y x))
	)


(defmethod .% ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (mod-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .% ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (mod-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))


;Less than

(defgeneric .< (x y))

(defmethod .< ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (less-than x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .< ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (less-than-tensor-scalar x y))
	)

(defmethod .< ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (less-than-tensor-scalar x y))
	)
 
(defmethod .< ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (less-than-tensor-scalar y x))
	)

(defmethod .< ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (less-than-tensor-scalar y x))
	)


(defmethod .< ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (less-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .< ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (less-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))


;Greater than

(defgeneric .> (x y))

(defmethod .> ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (greater-than x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .> ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (greater-than-tensor-scalar x y))
	)

(defmethod .> ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (greater-than-tensor-scalar x y))
	)

(defmethod .> ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (greater-than-tensor-scalar y x))
	)

(defmethod .> ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (greater-than-tensor-scalar y x))
	)


(defmethod .> ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (greater-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .> ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (greater-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))


;Less or equal than

(defgeneric .<= (x y))

(defmethod .<= ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (less-equal-than x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .<= ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (less-equal-than-tensor-scalar x y))
	)

(defmethod .<= ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (less-equal-than-tensor-scalar x y))
	)

(defmethod .<= ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (less-equal-than-tensor-scalar y x))
	)

(defmethod .<= ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (less-equal-than-tensor-scalar y x))
	)


(defmethod .<= ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (less-equal-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .<= ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (less-equal-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

;Greater or equal than

(defgeneric .>= (x y))

(defmethod .>= ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (greater-equal-than x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .>= ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (greater-equal-than-tensor-scalar x y))
	)

(defmethod .>= ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (greater-equal-than-tensor-scalar x y))
	)

(defmethod .>= ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (greater-equal-than-tensor-scalar y x))
	)

(defmethod .>= ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (greater-equal-than-tensor-scalar y x))
	)


(defmethod .>= ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (greater-equal-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .>= ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (greater-equal-than-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))




;Equal

(defgeneric .= (x y))

(defmethod .= ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (equal-val x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .= ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (equal-val-tensor-scalar x y))
	)

(defmethod .= ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (equal-val-tensor-scalar x y))
	)

(defmethod .= ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (equal-val-tensor-scalar y x))
	)

(defmethod .= ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (equal-val-tensor-scalar y x))
	)


(defmethod .= ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (equal-val-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .= ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (equal-val-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))


;Or

(defgeneric .or (x y))

(defmethod .or ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (or-val x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .or ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (or-val-tensor-scalar x y))
	)

(defmethod .or ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (or-val-tensor-scalar x y))
	)

(defmethod .or ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (or-val-tensor-scalar y x))
	)

(defmethod .or ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (or-val-tensor-scalar y x))
	)


(defmethod .or ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (or-val-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .or ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (or-val-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

;And

(defgeneric .and (x y))

(defmethod .and ((x scalar) (y scalar))
	(let* ((x-elements (slot-value x 'elements))
		   (y-elements (slot-value y 'elements))
		   (result (and-val x-elements y-elements)))
			 (make-instance 'scalar :tensor-elements result)
	))

(defmethod .and ((x tensor) (y scalar))
			 (make-instance 'tensor :tensor-elements (and-val-tensor-scalar x y))
	)

(defmethod .and ((x vector-tensor) (y scalar))
			 (make-instance 'vector-tensor :tensor-elements (and-val-tensor-scalar x y))
	)

(defmethod .and ( (y scalar) (x tensor))
			(make-instance 'tensor :tensor-elements (and-val-tensor-scalar y x))
	)

(defmethod .and ( (y scalar) (x vector-tensor))
			(make-instance 'vector-tensor :tensor-elements (and-val-tensor-scalar y x))
	)


(defmethod .and ((t1 tensor) (t2 tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'tensor :tensor-elements (and-val-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))

(defmethod .and ((t1 vector-tensor) (t2 vector-tensor))
	(let* ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements))
		  (t1-shape (slot-value (shape t1) 'elements))
		  (t2-shape (slot-value (shape t2) 'elements)))
			(if (equal t1-shape t2-shape) (make-instance 'vector-tensor :tensor-elements (and-val-tensor-tensor t1-elements t2-elements)) (error "Error: Tensors dont have the same size"))))