(define (iter-improve check improve)
    (define (iter x)
        (if (check x)
            x
            (iter (improve x))))
    iter)

(define (sqrt-x y)
    (define (good-enough? guess)
        (< (abs (- (* guess guess) y)) 0.001))
    (define (improve guess)
        (display guess)
        (average guess (/ y guess)))

    ((iter-improve good-enough? improve) 1.0))


(define (fixed-point-search f guess)
    (define (check guess)
        (< (abs (- guess (improve guess))) tolerance))
    (define (improve guess)
        (f guess))
    ((iter-improve check improve) guess))

(define (average x y)
    (/ (+ x y) 2))

(define (nth-root x n)
     (lambda (y)  (/ x (expt y (- n 1)))))

(define (my-sqrt x)
    (fixed-point-search (lambda (y) (average y (/ x y))) 1.0))
    


