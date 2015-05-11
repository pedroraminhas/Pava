(defparameter *current-value* -1)

(defun current-value (seq)
  (if (eq *current-value* (- (length seq) 1))
     (setf *current-value* 0)
    (setf *current-value* (+ *current-value* 1)))
  
  (setf result (nth *current-value* seq)))


(defun fillmatrix (lst seq)
  (let ((first-element (car lst)))
    (cond ((endp lst) ())
          ((numberp first-element) (cons (current-value seq) (fillmatrix (rest lst) seq)))
          ((listp first-element) (cons (fillmatrix first-element seq) (fillmatrix (rest lst) seq))))))

(defun new-dimension (dim v seq)
  (loop for x from 1 to dim 
     collect (if (eq x 1) 
                  v
                  (fillmatrix v seq))))

(defun without-last(lst)
  (cond ((null lst) ())
        ((null (cdr lst)) ())
        (t (cons (car lst) (without-last (cdr lst))))))

(defun make-tree-recursive (shape v seq)
  (let* ((last-el (nth (- (length v) 1) v))
         (new-dim ()))
   
    (if (not (endp shape))
      (setf new-dim (new-dimension (nth (- (length shape) 1) shape)
                         last-el
                         seq)))

    (if (endp shape)
        ()
    (cons new-dim
          (make-tree-recursive (without-last shape)
                                   (list new-dim)
                                   seq)))))

(defun make-tree (shape v seq)
  (cons v (make-tree-recursive shape (list v) seq)))

;É número primo?
(defun primep (value)
  (if (eq (length (dividers value)) 2)
    T
    nil))

;Divisores de um número
(defun dividers (value)
  (loop for x from 1 to value
        when (eq (mod value x) 0)
        collect x))


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
