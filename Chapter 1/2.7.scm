(define (lower-bound I)
    (car I))

(define (upper-bound I)
    (cdr I))

(display (upper-bound (make-interval 10 0.1)))
