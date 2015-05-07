;To load the file use (load "proj.lisp")

(defclass tensor ()
  ((elements :initarg :tensor-elements)
   (has :initarg :tensor-has)))

(defun s (y)
  (make-instance 'tensor :tensor-elements y :tensor-has "s"))

(defun v (&rest y)
  (make-instance 'tensor :tensor-elements y :tensor-has "v"))

(defun reshape (x y)
  (let* ((vector1 (slot-value x 'elements))
         (vector2 (slot-value y 'elements))
         (col-num (nth (- (length vector1) 1) vector1))
         (row-num (nth (- (length vector1) 2)  vector1))
         (reps (car vector1))
         (currentPos 0))
    (if (< (length vector1) 3)  ;Caso não seja especificado o número de repetições
      (setq reps 1))

    (make-instance 'tensor
      :tensor-elements (loop for i from 1 to reps
                         collect (loop for r from 1 to row-num
                           collect (loop for c from 1 to col-num
                             collect (nth currentPos vector2) do
                               (if (eq currentPos (- (length vector2) 1))
                                 (setq currentPos 0)
                               (setq currentPos (+ currentPos 1))))))
      :tensor-has "matrix")))

;Returns the symmetric of a number
(defun symmetric (x)
	(- 0 x))


(defun .-Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (symmetric it)))))))

(defun .- (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.-Aux x))
		  (t (make-instance 'tensor :tensor-elements (symmetric x-elements) :tensor-has "s")))))

;Returns tensor with elements from 1 to x
(defun interval (x)
	(let ((value ()))
		(dotimes (it x (make-instance 'tensor :tensor-elements value :tensor-has "v"))
			(setf value  (append value (list (+ it 1)))))))	

(defmethod print-object ((obj tensor) stream)
	(let ((tensor-type (slot-value obj 'has))) 
	(cond 
		((string= tensor-type "s") (format stream "~S" (slot-value obj 'elements)))
		((string= tensor-type "v") (format stream "~{~a~^ ~}" (slot-value obj 'elements)))
		((string= tensor-type "matrix") (format stream "~{~{~{~a~^ ~}~^ ~%~}~^ ~% ~%~}" (slot-value obj 'elements))))))

	


