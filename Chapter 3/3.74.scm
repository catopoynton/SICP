(define zero-crossings
  (stream-map 
    sign-change-detector
    sense-data
    (stream-cdr sense-data)))


(define (smooth s)
  (cons-stream (avg (car s) (cadr s))
    (average-stream (stream-cdr s))))

(define (avg a b)
  (/ (+ a b) 2))

(define (make-zero-crossings input-stream)
  (sign-change-detector (smooth s)))

(define (sign-change-detector s)
  (let ((s1 (stream-car s))
        (s2 (stream-cadr s)))
    (cons-stream 0
      (cond 
        ((and (>= 0 s1) (< 0 s2))
          -1)
         ((and (< 0 s1) (>= 0 s2))
          1)
        (else 0)))))