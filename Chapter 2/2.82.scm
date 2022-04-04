;for search coertions takes a list of lists, with each sub-list representing the types that a type can be coerced into.
;if there exists a single type that all the types can be coerced into, it will return that type, else false
(define (search-coercion coercions)
    (define (search-lists item lists)
        (cond ((null? lists) 
                    #t)
                ((element? item (car lists))
                    (search-lists item (cdr lists)))
                (else #f)))
    (if (null? (car coercions))
        #f
        (let ((type (car (car coercions))))
            (if (search-lists type coercions)
                type
                ;If there is a common type than it exists in the first type-list
                ;this horid line is cdr-ing down the first type-list and appening the reduced list to the other type-lists.
                (search-coercion (append (cdr (car coercions)) (cdr coercions)))))))

(display (search-coercion '((a b c) (b c d) (d a c))))