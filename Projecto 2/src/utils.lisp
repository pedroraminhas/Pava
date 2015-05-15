
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
                                  
(defun find-and-insert (lst deep it els)
  (let ((first-el (car lst))
        (first-el-els (car els)))
    (cond ((eq deep it) (append lst els))
          ((or (numberp first-el) (endp first-el)) ())
          ((listp first-el) (cons (find-and-insert first-el deep (+ it 1) first-el-els)
                                  (find-and-insert (rest lst) deep it (rest els)))))))  
        
(defun remove-elements (n list)
  (if (> n 0) 
      (remove-firsts n list)
    (remove-lasts n list)))

(defun remove-firsts (n list) ; remove n elementos do inicio da lista
  (cond ((eq n 0) list)
         (t (remove-firsts (- n 1) (rest list)))))

(defun remove-lasts (n list) ; remove n elementos do fim da lista
  (cond ((eq n 0) list)
        (t (remove-lasts (+ n 1) (butlast list)))))
        

(defgeneric transpose-tensor (x))

(defmethod transpose-tensor ((x vector-tensor))
  (let ((x-element (slot-value x 'elements))
         (result ()))
        (loop for x in x-element
            do (setf result (append result (list(list x)))))

        (make-instance 'tensor :tensor-elements result)))

(defmethod transpose-tensor ((x tensor))
  (let ((x-elements (slot-value x 'elements)))
        (make-instance 'tensor :tensor-elements (transpose-tensor-aux x-elements))))

(defun transpose-tensor-aux (z)
  (let ((result ())
          (result-aux ())
        (z-length (length z))
        (row-length (length (car z))))
        (progn (loop for x from 0 to (- row-length 1)
                      do (progn (loop for y from 0 to (- z-length 1)
                                     do (progn (setf result (append result (list (nth x (nth y z)))))))
                                     (progn (setf result-aux (append result-aux (list result)))
                                            (setf result (list)))))

                                     
        result-aux)))



(defun list-all (lst)
  (let ((first-element (car lst)))
  
    (cond ((endp lst) ())
          ((numberp first-element) (append (list first-element) (list-all (rest lst))))
          ((listp first-element) (append (list-all first-element) (list-all (rest lst)))))))
 
(defun fill-struct (struct elements)
  (let ((els elements))
    (defun fill-struct-els (struct)
      (cond ((atom struct) (pop els))
            ((null (cdr struct)) (list (fill-struct-els (car struct))))
            (t (append (list (fill-struct-els (car struct)))
                       (fill-struct-els (cdr struct))))))
    (fill-struct-els struct)))
 
(defun make-struct (dimensions cols rows)
  (if (car dimensions)
    (loop repeat (car dimensions) collect (make-struct (cdr dimensions) cols rows))
    (loop repeat rows collect (make-list cols))))
 
(defun make-circular (lst)
  (setf (cdr (last lst)) lst))

