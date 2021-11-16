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

(newline)
(display (reverse-l (list 1 2 3 4 6)))