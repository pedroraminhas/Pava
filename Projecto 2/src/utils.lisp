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
        
(defun find-and-remove (lst deep it range) ; remove o numero elementos especificado pelo range a uma dada deep
  (let ((first-el (car lst)))
    (cond ((eq deep it) (remove-elements range lst))
          ((or (numberp first-el) (endp first-el)) ())
          ((listp first-el) (cons (find-and-remove first-el deep (+ it 1) range)
                                  (find-and-remove (rest lst) deep it range))))))   
        
(defun remove-elements (n list) ; remove n elementos do inicio da lista
  (if (> n 0) 
      (remove-firsts n list)
    (remove-lasts n list)))

(defun remove-firsts (n list) ; remove n elementos do fim da lista
  (cond ((eq n 0) list)
         (t (remove-firsts (- n 1) (rest list)))))

(defun remove-lasts (n list)
  (cond ((eq n 0) list)
        (t (remove-lasts (+ n 1) (butlast list)))))
        



