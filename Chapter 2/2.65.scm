(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.61-2.scm")

(define (make-tree entry left right) (list entry left right))
(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))

(define (list->tree elements)
    (car (partial-tree elements (length elements))))
(define (partial-tree elts n) 
    (if (= n 0)
        (cons '() elts)
        (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result
            (partial-tree elts left-size)))
        (let ((left-tree (car left-result)) (non-left-elts (cdr left-result)) (right-size (- n (+ left-size 1))))
        (let ((this-entry (car non-left-elts)) (right-result (partial-tree (cdr non-left-elts) right-size)))
        (let ((right-tree (car right-result)) (remaining-elts (cdr right-result))) 
            (cons (make-tree 
                        this-entry
                        left-tree
                        right-tree) 
                remaining-elts))))))))

(define (tree->list tree)
    (define (copy-to-list tree result-list) 
        (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                (cons (entry tree) (copy-to-list (right-branch tree) result-list)))))
    (copy-to-list tree '()))


(define (t-intersection t1 t2)
    (let ((s1 (tree->list t1)) 
           (s2 (tree->list t2)))
        (list->tree (intersection-set s1 s2))))

(define t1 (list->tree (list 1 2 3)))
(define t2 (list->tree (list 1 5 6)))
(display (tree->list (t-intersection t1 t2)))