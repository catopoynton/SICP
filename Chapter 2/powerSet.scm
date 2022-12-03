(define (powerset s)
    (accumulate (lambda (x y) (append (list y)
                                        (map (lambda (z) (cons x z)) y)))
                                        (list '()) s))

(define (testing s)
(accumulate (lambda (x y)
    (append y
            (map (lambda (z) (cons x z)) y))) (list '()) s)) 
                        

(display (testing (list 1 2 3)))
