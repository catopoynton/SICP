;treemap!
;Map is doing the work of applying the operation to each item in the list
;cool how this setup will deal with any level of nesting of sub lists
(define (tree-map f tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree) 
            (tree-map f sub-tree) 
            (f sub-tree)))
            tree))

(define t2 (list 1
             (list 2 (list 3 4) 5)))

(define (square x)
    (* x x))

(display (tree-map square t2))
