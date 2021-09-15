(define (f n)
  (if (< n 3) n (+ (f (- n 1)) (* 2 (f(- n 2))) (* 3 (f (- n 3))))))

(define (new-f n)
  (f-iter 2 1 0 n))

(define (f-iter a b c count)
  (if (= count 0)
    c
    (f-iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

(define (sum-first-k k)
  (/ (* k (+ K 1)) 2))

(define (call-pascal n)
  (pascal n (level n 1)))

(define (pascal n l)
  (cond ((or (= n (+ (sum-first-k l) 1))  (= n (sum-first-k l)) (< n 3)) 1)
  (else (+ (pascal (- n l) (level (- n l) 3)) (pascal (- n l +1) (level (- n l +1) 3))))))

(define (level n k)
  (if (< n (sum-first-k k))
  (- k 1)
  (level n (+ k 1))))
