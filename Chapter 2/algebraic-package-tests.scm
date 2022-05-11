(load "/Users/catopoynton/Desktop/SICP/Chapter 2/number-package.scm")
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.84.scm")
(load-option 'format)
 
(define (assert-equal name proc ans . args)
    (let ((result (apply proc args)))
        (if (equal? ans result)
            '()
            (format #t "~S failed. Recived: ~s expected ~s" 
            name result ans))))

(define (scheme-number-tests)
    (assert-equal "test-zero" =zero? #t 0)
    (assert-equal "test-mul" mul -6 3 -2))

(define (complex-tests)
    (let ((c1 (make-complex-from-mag-ang 2 1)))
        (assert-equal "complex-zero-test" =zero? #t c1)))

(define (poly-tests)
    (let ((r1 (make-rat 2 3))
        (r2 (make-rat -1 2))
        (z1 (make-complex-from-real-imag 5 6))
        (var1 'x)
        (termlist-1 (list (list 0 0) (list 1 0) (list 2 0)))
        (termlist-2 (list (list 0 r2) (list 1 2) (list 2 3))))
        (let ((p1 (make-polynomial var1 termlist-1))
            (p2 (make-polynomial var1 termlist-2)))
            (assert-equal "poly-Zero" =zero? #t p1)
            (assert-equal "negate-poly" negate (make-polynomial var1 (list (list 0 -1) (list 1 -2) (list 2 -3))) p2)
            (display (=zero? (sub p2 p2))))))

(define (sparse-poly-tests)
    (make-sparse-polynomial 'x (list (list 0 0) (list 1 0) (list 2 0))))
    ;     (display p)))

    

(sparse-poly-tests)