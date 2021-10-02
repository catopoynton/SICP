(define (reverse items)
    (reverse-iter items (list)))

(define (reverse-iter items r)
    (if (null? items)
    r
    (reverse-iter (cdr items) (cons (car items) r))))

(define (deep-iter items r)
    (cond ((null? items) r)
          ((pair? (car items)) (deep-iter (cdr items) (cons (deep-reverse (car items)) r)))
          (else (deep-iter (cdr items) (cons (car items) r)))))
    
    

(define (deep-reverse items)
    (deep-iter items (list)))
(display (deep-reverse (list 23 72 (list 149 34))))