 (define (adjoin-set x set) 
   (cond ((null? set) (list x)) 
         ((= x (car set)) set) 
         ((< x (car set)) (cons x set)) 
         (else (cons (car set) (adjoin-set x (cdr set)))))) 
                
(define (union-set s1 s2)
    (cond 
        ((and (null? s1) (null? s2))
            '())
        ((null? s1)
            s2)
        ((null? s2)
            (s1))
        (else 
        (let ((x1 (car s1)) (x2 (car s2)))
            (cond   
            ((= x1 x2) 
                (cons x1 (union-set (cdr s1) (cdr s2))))
            ((< x1 x2)
                (cons x1 (union-set (cdr s1) s2)))
            ((> x1 x2)
                (cons x2 (union-set  s1 (cdr s2)))))))))

(define (intersection-set set1 set2) 
    (if (or (null? set1) (null? set2))
    '()
    (let ((x1 (car set1)) (x2 (car set2)))
        (cond 
            ((= x1 x2)
                (cons x1 (intersection-set (cdr set1) (cdr set2))))
            ((< x1 x2)
                (intersection-set (cdr set1) set2))
            ((< x2 x1)
                (intersection-set set1 (cdr set2)))))))   
    

                
