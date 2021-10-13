;funnily enough, by replacing append with cons you end up with an identity function. this is because you are 
;spliting the construct into its constituent parts, and then combining them back in exactly the same order!

(define (fringe data)
    (cond ((null? data) '())
          ((not (pair? data)) (list data))
          (else (append (fringe (car data)) (fringe (cdr data))))))

(define x (list (list 1 2) (list 3 4)))

(display (fringe x))