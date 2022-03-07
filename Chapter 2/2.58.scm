(load "/Users/catopoynton/Desktop/SICP/Chapter 2/symbolic-diff.scm")

(display "cat")
(define (sum? exp) 
    (and (pair? exp) (eq? (cadr exp) '+)))

(define (addend s) (car s))

(define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (cddr s)))

(define (make-sum a1 a2) 
        (cond ((=number? a1 0) a2) 
                ((=number? a2 0) a1)
                ((and (number? a1) (number? a2)) (+ a1 a2))
                (else (list a1 '+ a2))))

(define (add exp)
    (if (not (pair? exp)) exp 
    (make-sum (addend exp) (add (augend exp)))))


(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (cddr p)))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list m1 '* m2))))
                