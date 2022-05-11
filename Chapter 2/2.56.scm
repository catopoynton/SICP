;using a "wishful thinking" strategy - how would he like our program to evaluate symbolic expressions?
(define (deriv exp var) 
    (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0)) 
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp) 
            (make-sum
                (make-product (multiplier exp)
                    (deriv (multiplicand exp) var))
                (make-product (deriv (multiplier exp) var) 
                    (multiplicand exp))))
        ((exponential? exp)
            (make-product
                (make-product (exponent exp)
                    (make-exponential (base exp) (- (exponent exp) 1)))
                (deriv (base exp) var)))

        (else
        (error "unknown expression type: DERIV" exp))))

;x**n --> (* n x**(n-1))
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x) (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s)
    (if (null? (cdddr s))
        (caddr s)
        (cons '+ (cddr s))))

(define (product? x) (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p)
    (if (null? (cdddr p))
        (caddr p)
        (cons '* (cddr p))))

(define (=number? exp num) (and (number? exp) (= exp num)))

;Improved make sum adds expressions where both terms are numbers, and doesn't show 0 terms
(define (make-sum a1 a2) 
        (cond ((=number? a1 0) a2) 
                ((=number? a2 0) a1)
                ((and (number? a1) (number? a2)) (+ a1 a2))
                (else (list '+ a1 a2))))


(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2)) 
        (else (list '* m1 m2))))

(define (exponential? x)
    (and (pair? x) (eq? (car x) '**)))

(define (base exp) (cadr exp))

(define (exponent exp) (caddr exp))

(define (make-exponential b e)
    (cond ((= 0 e) 1)
        ((= 1 e) b)
        ((and (number? b) (number? e)) (expt b e))
        (else (list '** b e))))

