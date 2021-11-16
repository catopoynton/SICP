(define (square-tree tree) 
    (map (lambda (sub-tree)
        (if (pair? sub-tree) 
        (square-tree sub-tree) 
        (* sub-tree sub-tree)))
        tree))

(define t (list 1 (list 2 (list 3 4) 5) (list 6 7) 10))
(define t2 (list 1
             (list 2 (list 3 4) 5)
(list 6 7)))
(display (square-tree t2))