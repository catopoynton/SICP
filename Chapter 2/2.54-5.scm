(define (equal? a b)
    (or (eq? a b)
        (and 
            (pair? a)
            (pair? b)
            (equal? (car a) (car b))
            (equal? (cdr a) (cdr b)))))

(newline)
(display (equal? '(a) '(a)))
(equal? '(this is a list) '(this (is a) list))