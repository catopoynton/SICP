(define (sum term a next b ) 
(if (> a b)
    0
    (+ (term a)
        (sum term (next a) next b))))

(define (f-mult f k)
    (cond ((= k (or 0 n)) f)
        ((even? k) (* 2 f))
        (else (* f 4))))

(define(get-k a b n step)
    (/ (* (- step a) n) (- b a)))

(define (sum term a next b)
    (if (> a b)
        0
    (+ (term a) (sum term (next a) next b))))

(define (get-step a b n)
    (/ (- b a) n))

(define (get-integral f a b n)
    (s-integral f a b (get-step a b n)))

(define (s-integral f a b step)
    (define (add-step x)
        (+ x step))
    (* (/ step 3 ) (sum f a add-step b)))

(define (cube x)
    (* x x x))


(define (inc x) (+ x 1))

(define (i-sum term a next b)
    (define (iter a result)
        (if ( > a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))


