
(define (install-sparse-polynomial-package)
    ;; internal procedures
    ;; representation of poly

(define (variable p) (car p))

(define (term-list p) (cdr p))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))

    (define (make-sparse-poly variable term-list) 
        (cons variable term-list)) 
    
    (define (add-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2))
            (make-poly (variable p1)
                (add-terms (term-list p1) (term-list p2)))
            (error "Polys not in same var: ADD-POLY" (list p1 p2))))
    
    (define (sub-poly p1 p2)
        (add-poly p1 (negate-poly p2)))
    
    (define (mul-poly p1 p2)
        (if (same-variable? (variable p1) (variable p2)) 
            (make-poly (variable p1)
                (mul-terms (term-list p1) (term-list p2))) 
            (error "Polys not in same var: MUL-POLY" (list p1 p2))))

    (define (add-terms L1 L2)
        (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1) 
        (else
            (let ((t1 (first-term L1)) 
                (t2 (first-term L2)))
                (cond 
                    ((> (order t1) (order t2)) 
                        (adjoin-term
                            t1 (add-terms (rest-terms L1) L2))) 
                    ((< (order t1) (order t2))
                        (adjoin-term
                            t2 (add-terms L1 (rest-terms L2))))
                    (else 
                        (adjoin-term
                            (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                            (add-terms (rest-terms L1) (rest-terms L2)))))))))

    (define (mul-terms L1 L2) 
        (if (empty-termlist? L1) 
            (the-empty-termlist)
            (add-terms (mul-term-by-all-terms (first-term L1) L2) 
                                (mul-terms (rest-terms L1) L2))))
    (define (mul-term-by-all-terms t1 L) 
        (if (empty-termlist? L)
            (the-empty-termlist)
        (let ((t2 (first-term L)))
            (adjoin-term
            (make-term (+ (order t1) (order t2))
            (mul (coeff t1) (coeff t2))) (mul-term-by-all-terms t1 (rest-terms L))))))

    (define (adjoin-term term term-list) 
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))

    (define (=zero?-polynomial p)
        (newline)
        (cond 
            ((empty-termlist? (term-list p))
                #t)
            ((=zero? (coeff (first-term (term-list p))))
                (=zero?-polynomial (make-poly 
                                        (variable p) 
                                        (rest-terms (term-list p)))))
            (else
                #f)))

    (define (negate-poly p)
        (let ((neg-poly (make-poly (variable p) (list (list 0 -1)))))
            (mul-poly neg-poly p)))


    (define (the-empty-termlist) '())
    (define (first-term term-list) (car term-list))
    (define (rest-terms term-list) (cdr term-list)) 
    (define (empty-termlist? term-list) (null? term-list)) 
    (define (make-term order coeff) (list order coeff))
    (define (order term) (car term)) 
    (define (coeff term) (cadr term))

    ;; interface to rest of the system
    (define (tag p) (attach-tag 'sparse p))
    (put 'add '(sparse sparse)
        (lambda (p1 p2) (tag (add-poly p1 p2)))) 
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2)))) 
    (put 'make 'sparse
        (lambda (var terms) (tag (make-sparse-poly var terms))))
    (put 'negate '(polynomial)
        (lambda (p) (tag (negate-poly p))))
    (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put '=zero? '(polynomial)
        =zero?-polynomial)
        'done)

(define (install-dense-polynomial-package)
    (define (make-dense-poly variable term-list) 
        (cons variable term-list))

    ;;interface to the rest of the system
    (define (tag p) (attach-tag 'dense p))

    (put 'make 'dense
        (lambda (v t) (tag (make-dense-poly v t))))  
        'done
    (define (add-dense p1 p2)
        (add-term-lists (term-list p1) (term-listp2)))) 


(define (install-polynomial-package)
    (define (make-dense-polynomial variable termlist)
        ((get 'make 'dense) variable termlist))

    (define (make-sparse-polynomial variable termlist)
        ((get 'make 'sparse) variable termlist))
    
    (define (add-polynomial p1 p2)
        (display p1)
        (display"   ")
        (display p2)
        (add p1 p2))
    ;; interface to rest of the system
    (define (tag p) (attach-tag 'polynomial p))
    (put 'make-dense-polynomial 'polynomial
        (lambda (variable term-list) (tag (make-dense-polynomial variable term-list))))
    (put 'make-sparse-polynomial 'polynomial
        (lambda (variable term-list) (tag (make-sparse-polynomial variable term-list))))
    (put 'add '(polynomial polynomial)
         (lambda (p1 p2) (tag (add-polynomial p1 p2))))
'done)



