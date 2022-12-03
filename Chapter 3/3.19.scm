; The idea here is to have two pointers one that increments by 1, and the other by
; if there is a cycle, 1 will alway catch the other by the Chineese remainder theoram
(define (detect-cycle lst)
    (define (iter p1 p2)
        (cond 
            ((null? (cdr p2)) #f)
            ((eq?  p1 p2) #t)
            (else (iter (cdr p1) (cddr p2)))))
    (iter lst (cdr lst)))
        

(define (make-cycle x) (set-cdr! (last-pair x) x) x)

(define cycle (make-cycle (list 1 2 3 4)))

(display (detect-cycle (make-cycle (list 1 2 3 4))))