(define (sub-interval r1 r2)
    (make-interval (- (lower-bound r1) (upper-bound r2))
                   (- (upper-bound r2)  (lower-bound r1))))