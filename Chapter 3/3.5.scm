(define (estimate-integral p x1 y1 x2 y2 n)
  (define x 0)
  (define y 0)
  (define (integral-test)  
    (set! x (random-in-range x1 x2))
    (set! y (random-in-range y1 y2))
      (p x y))
  (let ((area (* (- x2 x1) (- y2 y1))))
      (* area (monte-carlo n integral-test))))

(define (random-in-range low high) 
  (let ((range (- high low)))
    (+ low (random range))))

(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond 
      ((= trials-remaining 0)
        (/ trials-passed trials))
      ((experiment)
        (iter (- trials-remaining 1) (+ trials-passed 1))) 
      (else
        (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))

  (define (cp x y)
    (<= (+ (expt x 2) (expt y 2)) 1))
 
 (estimate-integral cp -1.0 -1.0 1.0 1.0 100000)