(define (cont-frac n d k i)
    (display i)
    (if (= k i)
        (/ (n i) (d i))
        (/ 1 (+ (d i) (cont-frac n d k (+ i 1))))))




(define (cont-frac-iter n d k)
    (cont-iter n d k 0))

(define (cont-iter n d k result)
    (display k)
    (display " ")
    (display result)
    (newline)

    (if (= k 0)
    result
    (cont-iter n d (- k 1) (/ (n k) (+ (d k) result)))))


(define (d-e k)
    (display k)
    (if (= 2 (remainder k 3))
        (- k (floor (/ k 3)))
        1))


