(define (stream-enumerate-interval low high) (if (> low high)
the-empty-stream (cons-stream low
(stream-enumerate-interval (+ low 1) high))))

(define (show x) 
  (display x)
  (newline) 
  x)

(define x (stream-map show
  (stream-enumerate-interval 0 10)))

(define (stream-ref s n)
  (if (eq? n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (enumerate s n)
  (if (eq? n 0)
    (show (stream-car s))
    (begin 
      (show (stream-car s))
      (enumerate (stream-cdr s) (- n 1)))))

(define (add-streams s1 s2) (stream-map + s1 s2))

(define ones (cons-stream 1 ones))

(define integers
  (cons-stream 1 (add-streams ones integers)))

(define (scale-stream s m)
  (stream-map (lambda (x) (* x m)) s))
    
(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
  ((stream-null? s2) s1) 
  (else
    (let ((s1car (stream-car s1)) 
      (s2car (stream-car s2)))
      (cond ((< s1car s2car) 
        (cons-stream
          s1car
          (merge (stream-cdr s1) s2))) 
        ((> s1car s2car)
          (cons-stream
            s2car
            (merge s1 (stream-cdr s2))))
        (else (cons-stream
          s1car
          (merge (stream-cdr s1)
          (stream-cdr s2)))))))))

(define (interleave s1 s2) 
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                  (interleave s2 (stream-cdr s1)))))


(define (pairs s t) 
  (cons-stream
    (list (stream-car s) (stream-car t)) 
    (interleave
      (stream-map (lambda (x) (list (stream-car s) x)) 
        (stream-cdr t))
      (pairs (stream-cdr s) (stream-cdr t)))))