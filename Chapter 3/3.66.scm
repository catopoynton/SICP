(define (all-pairs s t) 
  (cons-stream
    (list (stream-car s) (stream-car t))
    (interleave
      (interleave
        (stream-map (lambda (x) (list (stream-car s) x)) 
          (stream-cdr t))
        (stream-map (lambda (x) (list x (stream-car t))) 
        (stream-cdr s)))
      (pairs (stream-cdr s) (stream-cdr t)))))


(define (missing-pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x)) t)
    (missing-pairs (stream-cdr s) (stream-cdr t))))

(define (triples s t u)
  (cons-stream
    (list (stream-car s) (stream-car t) (stream-car u))
    (interleave
      (stream-map (lambda (x) (cons (stream-car s) x)) (stream-cdr (pairs t u)))
      (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))
      
(define pythat-triples
  (stream-filter   
    (lambda (x) (= (+ 
                  (square (car x)) 
                  (square (cadr x)))
                  (square (caddr x))))
    (triples integers integers integers)))

(define (weighted-merge s t metric)
  (cond ((empty-stream? s) t)
    ((empty-stream? t) s)
    (else 
      (let ((car-s  (stream-car s))
            (car-t  (stream-car t)))
            (cond   ((< (metric car-s) (metric car-t))
                      (cons-stream car-s (weighted-merge (stream-cdr s) t metric)))
                    ((> (metric car-s) (metric car-t))
                      (cons-stream car-t (weighted-merge s (stream-cdr t) metric)))
                    (else 
                     (cons-stream car-t
                        (weighted-merge s (stream-cdr t) metric))))))))
(define (m p)
  (+ (car p) (cadr p)))

(define (ordered-pairs s t metric)
  (cons-stream 
    (list (car s) (car t))
    (weighted-merge 
       (stream-map (lambda (x) (list (stream-car s) x)) 
          (stream-cdr t))
        (ordered-pairs (stream-cdr s) (stream-cdr t) metric) 
      metric)))

(define (fizz-buzz-filter i)
  (and (not (divisible? i 2)) (not (divisible? i 3)) (not (divisible? i 5))))

(define (divisible? a b)
  (= 0 (remainder a b)))

(define fiz-ints (stream-filter fizz-buzz-filter integers ))

(define fizz-buzz
  (ordered-pairs fiz-ints fiz-ints
    (lambda (x) (+ (* 2 (car x)) (* 3 (cadr x)) (* 5 (car x) (cadr x))))))

(define ram-weight (lambda (x) (+ (expt (car x) 3) (expt (cadr x) 3))))

(define ram-ordered-pairs
  (ordered-pairs integers integers
    (lambda (x) (+ (expt (car x) 3) (expt (cadr x) 3)))))

(define (ram-pairs x)
  (let ((r1 (stream-ref x 0))
        (r2 (stream-ref x 1)))
          (if (= (ram-weight r1) (ram-weight r2))
            (cons-stream (list (ram-weight r1) r1 r2) 
                          (ram-pairs (stream-cdr x)))
            (ram-pairs (stream-cdr x)))))
  
(define rams (ram-pairs ram-ordered-pairs))

 