(load "/Users/catopoynton/Desktop/SICP/Chapter 2/accumulate.scm")
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.39.scm")

(define (queens board-size) 
    (define (queen-cols k)
        (if (= k 0)
            (list (empty-board board-size)) 
            (filter
                (lambda (positions) (safe? k positions)) 
                (flatmap (lambda (rest-of-queens) 
                        (map (lambda (new-row)
                            (adjoin-position new-row k rest-of-queens)) (enumerate-interval 1 board-size))) 
                (queen-cols (- k 1))))))
(queen-cols board-size))

(define (adjoin-position new-row k rest-of-queens)

;for each board in rest of queens the kth item of that board needs to have the value of new row
     (update rest-of-queens new-row (- k 1)))

;function to replace the value of a list at an index
(define (update lst val idx )
    (if (null? lst)
        lst
        (cons
            (if (= 0 idx)
                val
                (car lst))
                (update (cdr lst) val (- idx 1)))))

;returns an empty board represented as a list with each item in the list = -1
(define (empty-board board-size)
    (define (board-iter board length)
        (if (= 0 length)
            board
        (board-iter (append (list 0) board) (- length 1))))
    (board-iter (list) board-size))

;create a list whose elements are (i i+1 i+2 ... j) 
(define (enumerate-interval i j)
    (define (iter result j)
        (if (> i j)
            result
        (iter (cons j result) (- j 1))))
    (iter '() j))

;returns the item in position idx of a list
(define (get lst idx)
        (if (= 0 idx)
            (car lst)
            (get (cdr lst) (- idx 1))))

;checks if an element is a member of a list
(define (is-element? ele lst)
    (cond   ((null? lst) #f)
            ((= (car lst) ele) #t)
            (else (is-element? ele (cdr lst)))))

;returns a list of positions that are unsafe relative to column k.
(define (unsafe positions k)
    (define (unsafe-iter col lst remaining)
        (if (= col k)
            lst
        (let ((p (car remaining)))
            (unsafe-iter (+ col 1) (append lst (list p (+ p (- k col)) (- p (- k col)))) (cdr remaining)))))
    (unsafe-iter 1 (list ) positions))

;function finds the row of the queen in the kth column, and checks if she is safe with respect to the other queens
(define (safe? k positions)
    (let ((pos (get positions (- k 1)))
        (bad (unsafe positions k)))
        (not (is-element? pos bad)))) 




(define (count lst)
    (accumulate (lambda (x y) (+ 1 y)) 0 lst))

(count (queens 11))

;(display (add-row (list (empty-board 3) (empty-board 3)) 2 3))

