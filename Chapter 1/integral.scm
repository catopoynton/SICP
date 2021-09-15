(define (integral f a b dx) 
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
    dx))


(define (even? x)
    (= 0 (remainder x 2)))

(define (sum term a next b)
    (if (> a b)
        0
    (+ (term a) (sum term (next a) next b))))

(define (cube x)
    (* x x x))

(define (get-h a b n)
    (/ (- b a) n))

(define (get-integral f a b n)
    (s-integral f a b (get-h a b n)))

(define (s-integral f a b h)
    (define (add-h x)
        (+ x h))
    (sum (f-mult f count) (add-h a) add-h b 0))

(define(get-k a b n step)
    (/ (* (- step a) n) (- b a)))


(define (f-mult f k)
    (cond ((= k (or 0 n)) f)
        ((even? k) (* 2 f))
        (else (* f 4))))
        

    
(define (sum term a next b count ) 
(if (> a b)
    0
    (+ (term a)
        (sum term (next a) next b) (+ count 1))))