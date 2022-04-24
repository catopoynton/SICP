;This question poses something of a design challange - as we cannot simply compose the simplify proc with apply-generic as we do not what all generic operations 
;such as imag-part to attempt to be simplified. We need some way to decide whether a generic procedure should be simplified. The soloution below alters the definitions of
;only relevant generic operations, to simplify the returned value from apply-generic.
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/number-package.scm")

;simlify will lower an arguement to the lowest possible type
(define (simplify arg)
    (define (lower-arg arg)
        (if (get 'project (list (type-tag arg)))
            (let ((dropped-arg (project arg)))
                (let ((re-raised-arg (raise dropped-arg)))
                    (if (equal? arg re-raised-arg)
                        dropped-arg
                        arg)))
                        arg))
    (let ((lowered-arg (lower-arg arg)))
        (if (equal? (type-tag lowered-arg) (type-tag arg))
            arg ;if it was not possible to lower the arg, then return it
            (simplify lowered-arg))))

(define (get-proc op args)
    (let ((type-tags (map type-tag args)))
        (get op type-tags)))

(define (apply-generic op . args)
    (let ((proc (get-proc op args)))
    (if proc
            (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC" (list op (map type-tag args))))))


(define (add x y) (simplify (apply-generic 'add x y)))
(define (sub x y) (simplify (apply-generic 'sub x y))) 
(define (mul x y) (simplify (apply-generic 'mul x y))) 
(define (div x y) (simplify (apply-generic 'div x y)))

(define z1 (make-complex-from-real-imag 5 1))
(define z2 (make-complex-from-real-imag 3 1))
(display (sub z1 z2))