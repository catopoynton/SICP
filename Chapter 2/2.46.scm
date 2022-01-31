(define (make-vect x y)
    (cons x y)

(define (x-cor vect)
    (car vect)

(define (y-cor vect)
    (cdr vect)

(define (add-vect v1 v2)
    (make-vect (+ (x-cor v1) (x-cor v2))
                (+ (y-cor v1) (y-cor v2)))