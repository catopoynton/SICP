(load "/Users/catopoynton/Desktop/SICP/Chapter 2/accumulate.scm")
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.39.scm")


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
    (map (lambda (col) (update col new-row k)) rest-of-queens))

;function to replace the value of a list at an index
(define (update lst val idx )
    (if (null? lst)
        lst
        (cons
            (if (= 0 idx)
                val
                (car lst))
                (update (cdr lst) val (- idx 1)))))

;maps each board in "rest-of-queens" to a set of boards, where each board has a new row added to column k
(define (add-row rest-of-queens k board-size)
    (flatmap (lambda (new-row)
            (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size)))

;returns an empty board represented as a list with each item in the list = -1
(define (empty-board board-size)
    (define (board-iter board length)
        (if (= 0 length)
            board
        (board-iter (append (list -1) board) (- length 1))))
    (board-iter (list) board-size))

(define (enumerate-interval i j)
    (define (iter result j)
        (if (> i j)
            result
        (iter (cons j result) (- j 1))))
    (iter '() j))
        
(display (add-row (list (empty-board 3) (empty-board 3)) 2 3))


