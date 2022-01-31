(define (fold-left op initial sequence) 
    (define (iter result rest)
        (if (null? rest) result
            (iter (op result (car rest))
                (cdr rest))))
(iter initial sequence))

(define (reverse-r seq)
    (accumulate (lambda (x y) (append y (list x))) '() seq))

(define (reverse-l seq)
    (fold-left (lambda (x y) (append (list y) x)) '() seq))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (unique-pairs n)
    (flatmap (lambda (i)
        (map (lambda (j) (list i j)) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))


(define (enumerate-interval i j)
    (define (iter result j)
        (if (> i j)
            result
        (iter (cons j result) (- j 1))))
    (iter '() j))

(define (powerset s)
    (accumulate (lambda (x y) (append y
                                        (map (lambda (z) (cons x z)) y))) 
                                        list('()) s))

(display (unique-pairs 5))