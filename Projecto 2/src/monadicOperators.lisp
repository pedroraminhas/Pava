(defun fold (op)
  #'(lambda (x)
     (let* ((vec (slot-value x 'elements))
           (result (setf result (car vec))))
       (loop for i in vec
             for y from 0
             when (> y 0)
             do (setf result (slot-value (funcall op (s i) (s result)) 'elements)))
       result)))
