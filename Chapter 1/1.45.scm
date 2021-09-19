(define (average-damp f)
    (lambda (x) (/ (+ x (f x)) 2)))

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) 
    (fixed-point (newton-transform g) guess))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
        tolerance))

    (define (try guess)

    (let ((next (f guess)))
        (if (close-enough? guess next)
            next
            (try next))))
    (try first-guess))


(define (cubic a b c d)
    (lambda (x) (+ (* (expt x 3) a) (* (expt x 2) b) (* x c) d)))

(define (linear a)
    (lambda (x) (* a x)))

(define (cube x) (* x x x)) 


(define (inc x)
(+ x 1))

(define (double f)
    (lambda (x) (f (f x))))  

(define (fixed-point-transform f transform guess)
    (fixed-point (transform f) guess))

(define (nth-root x n)
     (lambda (y)  (/ x (expt y (- n 1)))))

(define (newton-n x n)
    (lambda (y) (- (expt y n) x)))

(define (repeated f n)
    (if (= n 1)
    f
    (repeated (compose f f) (- n 1))))

 (define (compose f g)
    (lambda (x) (f (g x))))


(define (find-root x n)
    (fixed-point-transform (nth-root x n) (repeated average-damp (floor (log-n n 2))) 1.0))

(define (log-n x n)
    (/ (log x) (log n)))

(display (find-root 13 32))

;(display (fixed-point-transform (nth-root 4 2) average-damp 1))








