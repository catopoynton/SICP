(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y))) 
        (p3 (* (upper-bound x) (lower-bound y))) 
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4) (max p1 p2 p3 p4))))

;modified to throw an error when y spans 0
(define (div-interval x y)
    (if (and (> 0 lower-bound y) (< 0 upper-bound y))
    (error "cannot divide by an interval that spans 0")
    (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
                   (/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))