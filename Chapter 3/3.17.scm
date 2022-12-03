(define (count-pairs x visited) 
  (cond
    ((not (pair? x)) visited)
    ((seen? x visited) visited)
    (else
      (let ((car-visits (count-pairs (car x) (cons x visited))))
        (count-pairs (cdr x) car-visits))))) 


(define (seen? x visited)
  (cond 
    ((null? visited) #f)
    ((eq? (car visited) x) #t)
    (else (seen? x (cdr visited)))))
