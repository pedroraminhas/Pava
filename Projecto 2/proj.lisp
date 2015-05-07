;To load the file use (load "proj.lisp")

;Definition of class Tensor

(defclass tensor ()
  ((elements :initarg :tensor-elements)
   (has :initarg :tensor-has)))


;Functions s and v
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

;Returns the inverse of a number
(defun inverse (x)
	(/ 1 x))

;Returns the factorial of a number
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1))) ) )

;Returns the negation of a number
(defun negation (x)
	(cond ((eq x 0) 1)
			(t 0)))

;Here are the aux functions called by the monadic functions
(defun .-Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (symmetric it)))))))

(defun ./Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (inverse it)))))))

(defun .!Aux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (factorial it)))))))

(defun .sinAux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (sin it)))))))

(defun .cosAux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (cos it)))))))

(defun .notAux (x)
	(let ((list-elements (slot-value x 'elements))
		(list-result ()))
	(dolist (it list-elements (make-instance 'tensor :tensor-elements list-result :tensor-has "v"))
		(setf list-result (append list-result (list (negation it)))))))



;MONADIC FUNCTIONS
(defun .- (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.-Aux x))
		  (t (make-instance 'tensor :tensor-elements (symmetric x-elements) :tensor-has "s")))))


(defun ./ (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(./Aux x))
		  (t (make-instance 'tensor :tensor-elements (inverse x-elements) :tensor-has "s")))))


(defun .! (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.!Aux x))
		  (t (make-instance 'tensor :tensor-elements (factorial x-elements) :tensor-has "s")))))

(defun .sin (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.sinAux x))
		  (t (make-instance 'tensor :tensor-elements (sin x-elements) :tensor-has "s")))))

(defun .cos (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.cosAux x))
		  (t (make-instance 'tensor :tensor-elements (cos x-elements) :tensor-has "s")))))

(defun .not (x)
	(let ((x-elements (slot-value x 'elements)))
	(cond ((listp x-elements)(.notAux x))
		  (t (make-instance 'tensor :tensor-elements (negation x-elements) :tensor-has "s")))))



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

	


