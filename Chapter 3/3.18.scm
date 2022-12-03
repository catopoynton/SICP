(define (detect-cycle pairs)
  (define (iter pairs visited)
    (cond   
      ((not (pair? pairs)) #f)
      ((memq pairs visited) #t)
      (else (iter (cdr pairs) (cons pairs visited)))))
  (iter pairs '()))

  (define (make-cycle x) (set-cdr! (last-pair x) x) x)
