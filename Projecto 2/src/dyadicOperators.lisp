(defun inner-product (op1 op2)
	#'(lambda (tensor1 tensor2)
		 (let* ((t2 (transpose-tensor tensor2))
		  	    (t2-elements (slot-value t2 'elements))
			    (t1-elements (slot-value tensor1 'elements)))
		 		(cond ((equal (class-of tensor1) (class-of(v 3))) (make-instance 'vector-tensor :tensor-elements (call-op t1-elements t2-elements op1 op2)))
		 				((equal (class-of tensor2) (class-of(v 3))) (make-instance 'vector-tensor :tensor-elements (call-op (list t1-elements) t2-elements op1 op2)))
		 			   (t (make-instance 'tensor :tensor-elements (call-op t1-elements t2-elements op1 op2)))))))

 (defun call-op (t1 t2 op1 op2)
 	(let ((result()))
 		(cond ((not (listp (car t1))) 	(progn 	(loop for y in t2
 													do (progn
 														(setf result (append result (list (return-result t1 y op1 op2))))))
 										result))
 				(t (loop for x in t1
 					 do (progn (setf result (append result (list (call-op x t2 op1 op2))))))))
 		result))




(defun return-result (x y op1 op2)
	(let* ((t1 (make-instance 'tensor :tensor-elements (list x)))
		  (t2 (make-instance 'tensor :tensor-elements (list y)))
		  (operation1 (slot-value (funcall op2 t1 t2) 'elements))
		  (operation1-vector (make-instance 'vector-tensor :tensor-elements (car operation1))))

		(slot-value (funcall (fold op1) operation1-vector) 'elements)))


