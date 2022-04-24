;for this exercise, he assume that all types are part of the tower hierarchy. This means that the raise-types procwill always return a pair of arguements that are of the same types.
;we would need a graph representation of the types to deal with disconected towers of types


(define (higher-type? a1 a2)
    (let ((raise-proc (get 'raise  (list (type-tag a1)))))
        (cond
            ((equal? (type-tag a1) (type-tag a2))
                #f)
            (raise-proc
                    (higher-type? (raise a1) a2))
            (else
            ;if the type cannot be raised further, than it must be the highest type - as we have already checked for equality 
                #t))))
            
(define (raise-args args)
    (map raise args))

;unify-types raises all the arguements in a list to the arguement of the highest type
(define (unify-types args)
    (define (unify-iter checked-args unchecked-args)
        (if (null? unchecked-args)
            checked-args
            (let ((checked-type (type-tag (car checked-args)))
                (unchecked-type (type-tag (car unchecked-args)))
                (checked-arg (car checked-args))
                (unchecked-arg (car unchecked-args)))
                (cond
                    ((equal? checked-type unchecked-type)
                        (unify-iter (cons unchecked-arg checked-args) (cdr unchecked-args)))
                    ((higher-type? checked-arg unchecked-arg)
                        (let ((raised-arg (car (raise-args (list unchecked-arg)))))
                            (unify-iter checked-args (cons raised-arg (cdr unchecked-args)))))
                    (else 
                        (let ((raised-args (raise-args checked-args)))
                            (unify-iter raised-args unchecked-args)))))))
    (unify-iter (list (car args)) (cdr args)))

(define (get-proc op args)
    (let ((type-tags (map type-tag args)))
        (get op type-tags)))

;apply-generic will first try to apply the op to the args, and if no operation for those arg types exist, apply-generic will attempt to unify the types of the args
;and apply the operation to the type-unified args
(define (apply-generic op . args)
    (let ((proc (get-proc op args)))
    (if proc
        (apply proc (map contents args))
        (let ((unified-args (unify-types args)))
            (let ((unified-proc (get-proc op unified-args)))
            (if unified-proc
                (apply unified-proc (map contents unified-args))
                (error "No method for these types: APPLY-GENERIC" (list op (map type-tag args)))))))))

            
