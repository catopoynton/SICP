(define (sqrt-stream x) 
  (define guesses (cons-stream 1.0
    (stream-map (lambda (guess) (sqrt-improve guess x)) guesses)))
  guesses)
(define (sqrt-improve guess x) 
  (average guess (/ x guess)))

(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(define (stream-limit s t)
  (let  ((diff (abs (- (stream-ref s 1)
                      (stream-ref s 2)))))
  (if (<= diff t)
    (stream-ref s 2)
    (stream-limit (stream-cdr s) t)))) 

(define (log-summands n)
  (cons-stream (/ 1.0 n)
    (stream-map - (log-summands (+ n 1)))))

(define log-2 
  (partial-sums (log-summands 1)))