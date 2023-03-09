
(define (integral delayed-integrand initial-value dt) 
  (define int
    (cons-stream
      initial-value
      (let ((integrand (force delayed-integrand)))
        (add-streams (scale-stream integrand dt) int)))) 
  int)

(define (solve f y0 dt)
  (define y (integral (delay dy) y0 dt)) 
  (define dy (stream-map f y))
y)

(define (solve-2nd a b dt y0 dy0)
  (define ddy (add-stream
                (scale-stream dy a)
                (scale-stream y b)))
  (define dy (delay (integral ddy dy0 dt)))
  (define y (delay (integral dy y0 dt)))
  y)

(define (solve-general-2nd f dt y0 dy0)
  (define ddt (stream-map f dy d))
  (define dy (delay (integral ddt dy0 dt)))
  (define y (delay (integral dy yo dt))))

(define (RLC R L C dt)
  (lambda (v0 i0)
    (define il (integral (delay dil) i0 dt))
    (define vc (integral (delay dvc) v0 dt))
    (define dil (add-streams (scale-stream vc (/ 1 L)) 
                            (scale-stream il (/ (* -1 R) L))))
    (define dvc (scale-stream il (/ -1 C)))
    (cons vc il)))

(define setup (RLC 1 1 .2 .1))
(define circut (setup 0 10))