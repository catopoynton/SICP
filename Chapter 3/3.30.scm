(define (ripple-carry-adder A B S c)
    (let ((C (map (lambda (x) (make-wire)) a-list)))
        (c0 (make-wire)))
        (map full-adder A B C S)
