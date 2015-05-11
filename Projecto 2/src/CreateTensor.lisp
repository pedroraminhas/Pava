;Definition of class Tensor
(defclass tensor ()
  ((elements :initarg :tensor-elements)))

;Definition of class scalar
(defclass scalar (tensor)
	((elements :initarg :tensor-elements)))

;Definition of class vector
(defclass vector-tensor (tensor)
	((elements :initarg :tensor-elements)))

;Definition of class matrix
(defclass matrix (tensor)
	((elements :initarg :tensor-elements)))



;Functions s, v and reshape
(defun s (y)
  (make-instance 'scalar :tensor-elements y))

(defun v (&rest y)
  (make-instance 'vector-tensor :tensor-elements y))

(defun reshape (v1 v2)
  (defparameter *current-value* -1)
  (let* ((vec1 (slot-value v1 'elements))
         (vec2 (slot-value v2 'elements))
         (cols-num (nth (- (length vec1) 1) vec1))
         (solution-tree (make-tree (without-last vec1)
                                     (loop for x from 1 to cols-num 
                                       collect (current-value vec2))
                                     vec2))
         (final-solution (nth (- (length solution-tree) 1) solution-tree)))
         (if (not (listp (car final-solution)))  (make-instance 'vector-tensor :tensor-elements final-solution)
         (make-instance 'tensor :tensor-elements final-solution) )))
