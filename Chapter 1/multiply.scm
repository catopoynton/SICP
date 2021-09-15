(define (multiply term a b next)
    (if (> a b)
    1
    (* (term a)
        (multiply term (next a) b next))))

(define (inc x) (+ x 1))

(define (id x) x)

(define (pi-a n)
    (/ (* 2 n) (+ 1 (* 2 n))))

(define (pi-b n)
    (/ (+ 2 (* 2 n)) (+ 1 (* 2 n))))
     
(define (get-pi b)
    ( * 4
        (multiply pi-a 1 b inc)
        (multiply pi-b 1 b inc)))

(define (accumulate comb null term a next b)
    (if (> a b)
        null
        (comb (term a) (accumulate comb null term (next a) next b))))

(define (accumulate-i comb null term a next b)
    (acc-iter comb term a next b null))

(define (acc-iter comb term a next b result)
    (if (> a b)
        result
        (acc-iter comb term (next a) next b (comb result (term a)))))

(define (filtered-accumulate comb null term a next b filter)
    (if (> a b)
        null
        (if (filter (term a))
            (comb (term a) (filtered-accumulate comb null term (next a) next b filter))
            (filtered-accumulate comb null term (next a) next b filter))))

(define (cube x)
    (* x x x))

(define (even? x)
    (= 0 (remainder x 2)))

