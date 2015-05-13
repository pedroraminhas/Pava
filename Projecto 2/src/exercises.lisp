;Tally
(defun tally (tensor)
	(funcall (fold #'.*)(shape tensor)))

;Rank function
(defun rank (tensor)
	(s (length (slot-value (shape tensor) 'elements))))

(defun primes ((x scalar))
  (let ((value (slot-value x 'elements)))
    (make-instance 'vector-tensor 
                   :tensor-elements (loop for x from 2 to value
                                          when (primep x)
                                          collect x))))

