(define (powerset s)
    (accumulate (lambda (x y) (append (list y)
                                        (map (lambda (z) (cons x z)) y)))
                                        (list '()) s))
; (newline)
; (display (powerset list(1)))
; (define a (list '() '(1)))

(define (testing s)
(accumulate (lambda (x y)
    (append y
            (map (lambda (z) (cons x z)) y))) (list '()) s)) 
                        

(display (testing (list 1 2 3)))
;(display (append a (map (lambda (z) (cons 3 z)) a)))