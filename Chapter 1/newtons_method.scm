(define (average-damp f)
    (lambda (x) (average x (f x))))

(define (deriv g)
    (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define dx 0.00001)

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess) (fixed-point (newton-transform g) guess))

(define tolerance 0.000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess)
  (display guess)
  (newline)
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
(display ((deriv cube) 5))

(define (inc x)
    (+ x 1))

(define (double f)
    (lambda (x) (f (f x))))  

 (((double (double (double double))) inc) 5)

 (define (compose f g)
    (lambda (x) (f (g x))))










