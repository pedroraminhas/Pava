(defun fold (op)
  #'(lambda (x)
     (let* ((vec (slot-value x 'elements))
           (result (setf result (car vec))))
       (loop for i in vec
             for y from 0
             when (> y 0)
             do (setf result (slot-value (funcall op (s i) (s result)) 'elements)))
       (s result))))


(defun scan (op)
  #'(lambda (x)
      (let* ((vec (slot-value x 'elements))
            (result (setf result (append () (list (car vec))))))
         (loop for i in vec
               for y from 0
               when (> y 0)
               do (setf result 
                        (append result 
                                (list (slot-value (funcall op (s i) 
                                                        (s (nth (- (length result) 1) result)))
                                            'elements)))))
          (make-instance 'tensor :tensor-elements (list result)))))

(defun outer-product (op)
  #'(lambda (lst tensor-cpy)
      (let ((t1 (slot-value lst 'elements))
            (t2 (slot-value tensor-cpy 'elements)))
      (defun fillt (lst tensor-cpy)
        (let ((first-element (car lst)))
          (cond ((endp lst) ()) 
                ((numberp first-element)      
                  (cons (funcall (fill-with-value op) tensor-cpy first-element) (fillt (rest lst) tensor-cpy)))
                ((listp first-element) (cons (fillt first-element tensor-cpy) (fillt (rest lst) tensor-cpy)))))) 
     
       (make-instance 'tensor :tensor-elements (fillt t1 t2)))))

; função auxiliar no calculo de cada combinação
(defun fill-with-value (op)
  #'(lambda (lst value)
      (defun fillw (lst value)
        (let ((first-element (car lst)))
          (cond ((endp lst) ())
                ((numberp first-element)                     
                  (cons (slot-value (funcall op (s value) (s first-element)) 'elements) (fillw (rest lst) value)))
                ((listp first-element) (cons (fillw first-element value) (fillw (rest lst) value)))))) (fillw lst value)))

