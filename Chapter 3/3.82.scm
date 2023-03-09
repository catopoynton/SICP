(define (monte-carlo experiment-stream passed failed) 
  (define (next passed failed)
    (cons-stream
      (/ passed (+ passed failed)) 
      (monte-carlo
        (stream-cdr experiment-stream) passed failed))) 
  (if (stream-car experiment-stream)
    (next (+ passed 1) failed)
    (next passed (+ failed 1))))

(define (integral-stream rand-stream-x rand-stream-y f test)
  (stream-map (lambda (x y) (test (f x y))) rand-stream-x rand-stream-y))

(define (random-in-range low high) 
  (let ((range (- high low)))
    (cons-stream (+ low (random range))
      (random-in-range low high))))

(define (estimate-integral f test x1 y1 x2 y2)
  (let ((area (* (- x2 x1) (- y2 y1))))
    (scale-stream 
      (monte-carlo (integral-stream 
                      (random-in-range x1 x2) 
                      (random-in-range y1 y2)
                      f
                      test)
                    0 0)
      area)))

(define (circle-func x y)
  (+ (expt x 2) (expt y 2)))

(enumerate (estimate-integral circle-func (lambda (x) (<= x 1)) -1.0 -1.0 1.0 1.0) 10000)
                
