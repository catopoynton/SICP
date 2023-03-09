; integrand here represents a sum of integrated values. i.e. x has been passed
; through an integrating function
(define (integral integrand initial-value dt) 
  (define int
    (cons-stream initial-value
      (add-streams (scale-stream integrand dt)
      int)))
  int)

(define (RC R C t)
  (lambda (i v0)
    (add-streams (scale-streams i R)
      (scale-streams (integral i v0 t) (/ 1 C)))))
