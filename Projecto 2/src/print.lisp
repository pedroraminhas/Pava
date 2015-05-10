;Print object	
(defmethod print-object ((obj scalar) stream) 
	(format stream "~S" (slot-value obj 'elements)))

(defmethod print-object ((obj vector-tensor) stream) 
	(format stream "~{~a~^ ~}" (slot-value obj 'elements)))

(defmethod print-object ((obj matrix) stream)
	(let ((tensor-elements (slot-value obj 'elements)))
		(loop for x in tensor-elements
			  for y from 1

			 when (> y 1)
			 do (format stream "~%~%")

     		 do (format stream "~{~a~^ ~}" x))))


(defun print-list-of-lists (elements stream depth final)
	(progn (loop for x in elements
				 for y from 1

				when (> y 1)
					 do (format stream "~%")

				do (format stream "~{~a~^ ~}" x))
			))

;Prints the number of wanted newlines
(defun print-space (stream nSpace)
	(loop for y from 0 to nSpace
				do (format stream "~%")))


;Aux method of Tensor
(defun print-object-aux (obj stream depth total-depth final)
	(let ((tensor-elements (slot-value obj 'elements))
		 (depth-aux depth))
	(cond ((not (listp (car (car tensor-elements)))) (print-list-of-lists tensor-elements stream (- total-depth depth) final))
			(t (progn (setf depth (+ depth 1))
					(loop for x in tensor-elements
						  for i from 1
						  do (progn 
						  			(if (eq i (length tensor-elements)) 
						  				(progn (print-object-aux (make-instance 'tensor :tensor-elements x) stream depth total-depth t))
						  				(progn (print-object-aux (make-instance 'tensor :tensor-elements x) stream depth total-depth NIL)
						  					(print-space stream (- total-depth depth))))
						  					)))))))


;Counts the depth of the tensor
(defun depth-count (lista)
	(cond ((not (listp (car (car lista))))  1)
			(t (+ 1 (depth-count (car lista))))))

;Prints an Tensor
(defmethod print-object ((obj tensor) stream)
	(let* ((tensor-elements (slot-value obj 'elements))
		   (total-depth  (depth-count tensor-elements)))
		(print-object-aux obj stream 0 total-depth NIL)))

