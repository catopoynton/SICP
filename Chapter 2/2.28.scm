(define (fringe data)
    (cond ((null? data) (list))
          ((not (pair? data)) data)
          (null? (cdr data)) (fringe (car data))
          (else (cons (fringe (car data)) (fringe (cdr data))))))

(define x (list (list 1 2) (list 3 4)))

(display (fringe x))