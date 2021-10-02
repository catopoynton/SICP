(define (append list1 list2)
    (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))


(define (last-pair list)
    (if (null? (cdr list))
    list
    (last-pair (cdr list))))


(define (reverse list1)
    (define (reverse-iter list1 r)
        (if (null? list1)
        r
        (reverse-iter (cdr list1) (cons (car list1) r))))
(reverse-iter list1 (list)))


(display (reverse (list 23 72 149 34)))