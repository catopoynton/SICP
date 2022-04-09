;attempts to find a type that all types can be coerced
;this does depend on get coertions having an identity proceddure for coercing types into their own type
(define (try-coercion types)
    (define (iter types count)
        (let ((type (check-types (car types) (cdr types))))
            (cond ((count >= arg-length)
                        #f)
                   (type type)
                   (else (try-coercion (append (cdr types) (list (car types))) (+ count 1))))))

    (let ((arg-length (length args)))
        (type (iter types 0))))
        

(define (check-types type types)
    (cond ((null? types)
            type)
        ((not (null? (get-coertion type (car types))))
            (check-types type (cdr types)))
        (else #f)))