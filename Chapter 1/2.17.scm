(define (append list1 list2)
    (if (null? list1)
    list2
    (cons (car list1) (append (cdr list1) list2))))



(define (last-pair list)
    (if (null? (cdr list))
    list
    (last-pair (cdr list))))



(define (reverse list)
    (if (null? (cdr list))
    list
    (cons (car (reverse (cdr list))) (car list))))


(display (reverse (list 23 72 149 34)))