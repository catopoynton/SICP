;Crazy stuff happning here! cons simply return a procedure that takes an arbitary procedure as an input
; where that input procedure takes the cons arguement as its own arguements.
;car/cdr then returns the cons procedure which takes as its arguement another procedure which given two arguements returns
;the first or seccond one respectivly!
;finally this will evaluate leaving us with one of the original arguements of are cons call.
(define (cons x y) 
    (lambda (m) (m x y)))

(define (car z)
    (z (lambda (p q) p)))

(define (cdr z)
    (z (lambda (p q) q)))

