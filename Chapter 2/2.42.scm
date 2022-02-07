(define (queens board-size) 
    (define (queen-cols k)
        (if (= k 0)
            (list empty-board) 
            (filter
                (lambda (positions) (safe? k positions)) 
                (flatmap (lambda (rest-of-queens) 
                        (map (lambda (new-row)
                            (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size))) 
                (queen-cols (- k 1))))))
(queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)
;for each board in rest of queens the kth item of that board needs to have the value of new row
    (map (lambda (col) (append col (list new-row))) rest-of-queens))

;function to replace the value of a list at an index
(define (update l1 item idx )
    (define (update-iter head tail pos)
        (if (= pos idx)
            (append (append (list item) head) (cdr tail))
        (update-iter (append (list (car tail)) head) (cdr tail) (+ pos 1))))
    (update-iter '() l1 0))

(define (add-row rest-of-queens k board-size)
    (map (lambda (new-row)
            (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size)))

(display (update (list "dog" "bumbi" 1) "cat" 0))


