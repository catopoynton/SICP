(define (make-rat n d)
    (if (< d 0)
        (cons (* -1 n) (* -1 d))
        (cons n d)))

(define (numer x)(car x))
(define (denom x) (cdr x))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
              (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
    (* (denom x) (denom y)))) 

(define (div-rat x y)
    (make-rat (* (numer x) (denom y)) 
    (* (denom x) (numer y))))


(define (print-rat x) 
    (newline)
    (display (numer x)) 
    (display "/")
     (display (denom x)))

(print-rat (add-rat (make-rat -1 3) (make-rat 1 -2)))