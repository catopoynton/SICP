
(define (stateful-rand)
  (let ((x 0))
    (define (generate)
      (begin (set! x (rand-update x)) x))
    (define (reset new-val)
     (set! x new-val))))

(define (stream-rand init-val)
  (define the-stream
    (cons-stream (random init-val)
      (stream-rand (stream-car the-stream))))
  the-stream)
