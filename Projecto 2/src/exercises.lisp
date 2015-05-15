;Tally
(defun tally (tensor)
	(funcall (fold #'.*)(shape tensor)))

;Rank function
(defun rank (tensor)
	(funcall (fold #'.+)(.and (shape tensor) (s 1))))

;Within function
(defun within (tensor scalar1 scalar2)
	(select (.and (.>= tensor scalar1) (.<= tensor scalar2)) tensor))


(defun primes (x)
  (let ((value (slot-value x 'elements)))
    (make-instance 'vector-tensor 
                   :tensor-elements (loop for x from 2 to value
                                          when (primep x)
                                          collect x))))

