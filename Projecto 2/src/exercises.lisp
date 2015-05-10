;Rank function
(defun rank (tensor)
	(s (length (slot-value (shape tensor) 'elements))))