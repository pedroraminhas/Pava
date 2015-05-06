;To load the file use (load "proj.lisp")

(defclass tensor ()
	((elements	:initarg :tensor-elements)
	(has :initarg :tensor-has)
		))

(defun s (y)
	(make-instance 'tensor :tensor-elements y :tensor-has "s"))

(defun v (&rest y)
	(make-instance 'tensor :tensor-elements y :tensor-has "v"))

(defun reshape (x y)
	(make-instance 'tensor :tensor-elements y :tensor-has "matrix"))


(defmethod print-object ((obj tensor) stream)
	(let ((tensor-type (slot-value obj 'has))) 
	(cond 
		((string= tensor-type "s") (format stream "~S" (slot-value obj 'elements)))
		((string= tensor-type "v") (format stream "~{~a~^ ~}" (slot-value obj 'elements)))
		((string= tensor-type "matrix") (format stream "~{~a~^ ~}" (slot-value obj 'elements))))))

	


