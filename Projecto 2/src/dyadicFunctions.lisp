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
			
(defun reshape (new-shape elements)
  (let* ((new-shape (slot-value new-shape 'elements))
         (elements (slot-value elements 'elements))
         (size (last new-shape 2))
         (rows (car size))
         (cols (cadr size))
         (struct (butlast new-shape 2))
         (final-solution ()))
    (setf elements (list-all elements))
    (setf final-solution (fill-struct (make-struct struct cols rows) (make-circular elements)))
    (if (and (eq (length (tensor-dims final-solution)) 2)
             (eq (car (tensor-dims final-solution)) 1))
        (make-instance 'vector-tensor :tensor-elements (car final-solution))
      (make-instance 'tensor :tensor-elements final-solution))))


(defgeneric drop (x y))

(defmethod drop ((x scalar) (y vector-tensor))
  (let ((value (slot-value x 'elements))
        (tensor-elements (slot-value y 'elements)))
    (cond ((eq value 0) y)
          ((> value 0) (drop (make-instance 'scalar :tensor-elements (- value 1))
                             (make-instance 'vector-tensor
                                             :tensor-elements (cdr tensor-elements))))
          ((< value 0) (drop (make-instance 'scalar :tensor-elements (+ value 1))
                             (make-instance 'vector-tensor
                                             :tensor-elements (butlast tensor-elements)))))))

(defmethod drop ((x vector-tensor) (y vector-tensor))
  (let ((vals (slot-value x 'elements)))
    (cond ((endp vals)  y)
          ((eq (length vals) 1) (drop (make-instance 'scalar :tensor-elements (car vals)) y))
          ((eq (length vals) 2) (if (eq (car vals) 0)
                                    (drop (make-instance 'scalar 
                                                         :tensor-elements (car (cdr vals))) y)
                                  (make-instance 'vector-tensor 
                                                 :tensor-elements nil)))
        ((> (length vals) 2) (error "RANK ERROR"))))) ; porque um vector só tem 1 dimensao

(defmethod drop ((x scalar) (y tensor))
  (let* ((value (slot-value x 'elements))
       (tensor-elements (slot-value y 'elements))
       (tensor-len (length  tensor-elements)))
    (cond ((eq value 0) y)
          ((and (> value 0) (< value tensor-len)) 
             (drop (make-instance 'scalar :tensor-elements (- value 1))
                   (make-instance 'tensor :tensor-elements (cdr tensor-elements))))
          ((and (< value 0) (< (symmetric value) tensor-len)) 
            (drop (make-instance 'scalar :tensor-elements (+ value 1))
                  (make-instance 'tensor :tensor-elements (butlast tensor-elements))))
          (t (make-instance 'vector-tensor :tensor-elements nil)))))

(defmethod drop ((x vector-tensor) (y tensor))
  (let* ((vals (slot-value x 'elements))
        (tensor-elements (slot-value y 'elements))
        (x-rank (length vals))
        (y-rank (length (tensor-dims tensor-elements)))
        (solution tensor-elements))
     
     (cond ((<= x-rank y-rank)
       (loop for range in vals 
             for deep from 0 
             ;if (<= val-len (length (tensor-dims tensor-elements))) 
             do (setf solution (find-and-remove solution deep 0 range))))
       (t (error "RANK ERROR")))

      (make-instance 'tensor :tensor-elements solution)))


;Select


(defgeneric select (x y))

(defmethod select ((t1 scalar) (t2 scalar))
	(let ((t1-element (slot-value t1 'elements))
		  (t2-element (slot-value t2 'elements)))
			(if (equal t1-element 1) (s t2-element) (s 0))
	))

(defmethod select ((t1 tensor) (t2 vector-tensor))
	(let ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements)))
			(make-instance 'vector-tensor :tensor-elements (select-aux t1-elements t2-elements))))

(defmethod select ((t1 tensor) (t2 tensor))
	(let ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements)))
			(make-instance 'tensor :tensor-elements (select-aux t1-elements t2-elements))))

(defun select-aux (t1 t2)
	(let ((t1-size (length (slot-value (shape  (make-instance 'tensor :tensor-elements t1)) 'elements)))
		  (t2-size (length (slot-value (shape  (make-instance 'tensor :tensor-elements t2)) 'elements)))
		  (result ()))
	(cond ((equal t1-size t2-size) (setf result (append result (select-list t1 t2))))
		  (t (loop for x in t2
		  		do (setf result (append result (list (select-aux t1 x)))))))
	result))

(defun select-list (t1 t2)
	(let ((result-aux ())
               (result ()))
	(cond ((not (listp (car t2))) (loop for x in t1
										for y in t2
										do (if (not (equal x 0)) (setf result-aux (append result-aux (list y))))))
			(t (loop for x in t1
					 for y in t2
				do (setf result (append result-aux (list (select-list x y)))))))
	result-aux))


;Member

(defgeneric member? (t1 t2))

(defmethod member? ((t1 scalar) (t2 scalar))
	(let ((t1-element (slot-value t1 'elements))
		  (t2-element (slot-value t2 'elements)))
			(s (equal-val t1-element t2-element))))


(defmethod member? ((t1 tensor) (t2 tensor))
	(let ((t1-element (slot-value t1 'elements))
		  (t2-element (slot-value t2 'elements)))
			(make-instance 'tensor :tensor-elements (member-aux t1-element t2-element))
	))

(defmethod member? ((t1 vector-tensor) (t2 tensor))
	(let ((t1-element (slot-value t1 'elements))
		  (t2-element (slot-value t2 'elements)))
			(make-instance 'vector-tensor :tensor-elements (member-aux t1-element t2-element))
	))

(defmethod member? ((t1 scalar) (t2 tensor))
	(let ((t1-elements (slot-value t1 'elements))
		  (t2-elements (slot-value t2 'elements)))
	(s (compare-scalar-tensor t1-elements t2-elements))))


(defun member-aux (t1 t2)
	(let ((result ()))
		(cond ((not (listp (car t1))) (setf result (append result (compare-vector-tensor t1 t2))))
			   (t (loop for x in t1
			   		do  (setf result (append result (list (member-aux x t2)))
		))))
		result))

(defun compare-vector-tensor (t1 t2)
	(let ((result-aux ()))
		(loop for x in t1
			do (setf result-aux (append result-aux (list (compare-scalar-tensor x t2))))
	)
		result-aux))

(defun compare-scalar-tensor (t1 t2)
	(let ((results 0))
		(cond ((not (listp (car t2))) (loop for y in t2
											do (if (equal t1 y) 
																		(setf results 1))))
				(t (loop for y in t2
						do (compare-scalar-tensor t1 y))))
		results))

(defgeneric  catenate (x y))

(defmethod catenate ((x scalar) (y scalar))
  (let ((t1 (slot-value x 'elements))
        (t2 (slot-value y 'elements)))
        (v t1 t2)))

(defmethod catenate ((x vector-tensor) (y vector-tensor))
  (let* ((t1 (slot-value x 'elements))
         (t2 (slot-value y 'elements))
         (merged ()))
         (setf merged (append (car (list t1)) (append (car (list t2)))))
    (make-instance 'vector-tensor :tensor-elements merged)))

(defmethod catenate ((x tensor) (y tensor))
  (let* ((t1 (slot-value x 'elements))
        (t2 (slot-value y 'elements))
        (rank-t1 (length (tensor-dims t1)))
        (rank-t2 (length (tensor-dims t2))))
     (if (eq rank-t1 rank-t2)
        (make-instance 'tensor :tensor-elements (find-and-insert (list t1) rank-t1 0 (list t2)))
       (error "ERROR LENGHT"))))
		
		






