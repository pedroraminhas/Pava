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
