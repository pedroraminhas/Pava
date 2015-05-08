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



;Functions s and v
(defun s (y)
  (make-instance 'scalar :tensor-elements y))

(defun v (&rest y)
  (make-instance 'vector-tensor :tensor-elements y))