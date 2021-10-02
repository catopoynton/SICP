(define (all-but-last items)
    (define (iter items out)
        (if (null? (cdr items))
        out
        (iter (cdr items) (append out (list (car items))))))
    (iter items (list)))


(define (cc amount coin-values) 
    (cond ((= amount 0) 1)
    ((or (< amount 0) (no-more? coin-values)) 0) 
    (else
    (+ (cc amount (except-first-denomination
    coin-values)) (cc (- amount
    (first-denomination coin-values))
    coin-values)))))

(display (all-but-last (list 1 2 3 4 5)))