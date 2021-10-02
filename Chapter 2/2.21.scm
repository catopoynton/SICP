(define (square-list items)
    (if (null? items)
    (list)
    (cons (* (car items) (car items)) (square-list (cdr items)))))

(define (improve-square-list items)
    (map (lambda (x) (* x x)) items)) 

(define (map proc items)
    (if (null? items)
    (list)
    (cons (proc (car items)) (square-list (cdr items)))))


(define (for-each f iterable)
    (cond ((null? iterable) #f)
    ((f (car iterable))
    (for-each f (cdr iterable)))))

;(for-each (lambda (x) (newline) (display x)) (list 1 2 3 4 5))

(define sequence (list 1 3 (list 5 7) 9))
(display (car (cdr (car (cdr (cdr sequence))))))