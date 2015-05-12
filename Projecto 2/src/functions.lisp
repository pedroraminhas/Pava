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

(defun less-than (x y)
	(cond ((< x y) 1)
		  (t 0)))

(defun greater-than (x y)
	(cond ((> x y) 1)
		  (t 0)))

(defun less-equal-than (x y)
	(cond ((<= x y) 1)
		  (t 0)))

(defun greater-equal-than (x y)
	(cond ((>= x y) 1)
		  (t 0)))

(defgeneric subtraction-tensor-scalar (t1 t2))

(defmethod subtraction-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (- it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (subtraction-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod subtraction-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (- (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (subtraction-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun subtraction-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (- x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (subtraction-tensor-tensor x y)))))))
    result))


(defgeneric addition-tensor-scalar (t1 t2))

(defmethod addition-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (+ it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (addition-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod addition-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (+ (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (addition-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun addition-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (+ x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (addition-tensor-tensor x y)))))))
    result))

;Multiplication

(defgeneric multiplication-tensor-scalar (t1 t2))

(defmethod multiplication-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (* it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (multiplication-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod multiplication-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (* (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (multiplication-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun multiplication-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (* x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (multiplication-tensor-tensor x y)))))))
    result))

;Division

(defgeneric division-tensor-scalar (t1 t2))

(defmethod division-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (/ it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (division-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod division-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (/ (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (division-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun division-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (/ x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (division-tensor-tensor x y)))))))
    result))


;Integer Division

(defgeneric integer-division-tensor-scalar (t1 t2))

(defmethod integer-division-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (values (floor it (slot-value t2 'elements))))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (integer-division-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod integer-division-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (values (floor (slot-value t2 'elements) it)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (integer-division-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun integer-division-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (values (floor x y)))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (integer-division-tensor-tensor x y)))))))
    result))

;Remainder Integer Division

(defgeneric mod-tensor-scalar (t1 t2))

(defmethod mod-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (mod it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (mod-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod mod-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (mod (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (mod-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun mod-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (mod x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (mod-tensor-tensor x y)))))))
    result))

;Less Than

(defgeneric less-than-tensor-scalar (t1 t2))

(defmethod less-than-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (less-than it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (less-than-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod less-than-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (less-than (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (less-than-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun less-than-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (less-than x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (less-than-tensor-tensor x y)))))))
    result))


;Greater Than

(defgeneric greater-than-tensor-scalar (t1 t2))

(defmethod greater-than-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (greater-than it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (greater-than-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod greater-than-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (greater-than (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (greater-than-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun greater-than-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (greater-than x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (greater-than-tensor-tensor x y)))))))
    result))

;Greater Than

(defgeneric less-equal-than-tensor-scalar (t1 t2))

(defmethod less-equal-than-tensor-scalar ((t1 tensor) (t2 scalar))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (less-equal-than it (slot-value t2 'elements)))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (less-equal-than-tensor-scalar (make-instance 'tensor :tensor-elements x) t2)))))))
    result))

(defmethod less-equal-than-tensor-scalar ((t2 scalar) (t1 tensor))
  (let ((result ())
     (tensor-elements (slot-value t1 'elements)))
    (cond ((not (listp (car tensor-elements))) (dolist (it tensor-elements result) (setf result (append result (list (less-equal-than (slot-value t2 'elements) it))))))
        (t (loop for x in tensor-elements
          do (setf result (append result (list (less-equal-than-tensor-scalar t2 (make-instance 'tensor :tensor-elements x))))))))
    result))


(defun less-equal-than-tensor-tensor (t1 t2)
  (let ((result ()))
    (cond ((not (listp (car t1))) (loop for x in t1
                      for y in t2
                     do (setf result (append result (list (less-equal-than x y))))))
        (t (loop for x in t1
             for y in t2
          do (setf result (append result (list (less-equal-than-tensor-tensor x y)))))))
    result))