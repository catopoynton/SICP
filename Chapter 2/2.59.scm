(define (adjoin-set x set) 
    (if (element-of-set? x set)
        set
        (cons x set)))

(define (element-of-set? x set) 
    (cond ((null? set) false)
    ((equal? x (car set)) true)
    (else (element-of-set? x (cdr set)))))

(define (union-set s1 s2)
    (if (null? s1)
        s2
        (union-set (cdr s1) (adjoin-set (car s1) s2))))