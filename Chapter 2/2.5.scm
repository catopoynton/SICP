;pairs of non negative numbers
(define (cons a b)
    (lambda (m) (m (* (expt 2 a) (expt 3 b)))))

(define (get-largest-power i e x)
    (if (not (eq? (remainder x (expt e i)) 0))
        (- i 1)
    (get-largest-power (+ i 1) e x)))

(define (car z)
    (z (lambda (x) (get-largest-power 1 2 x))))

(define (cdr z)
    (z (lambda (x) (get-largest-power 1 3 x))))

(newline)
(display (car (cons 5 4)))
(newline)
(display (cdr (cons 5 4)))

