(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.39.scm")
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/accumulate.scm")

(define (make-leaf symbol weight) (list 'leaf symbol weight)) 
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right) 
        (list left right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree)) 
(define (right-branch tree) (cadr tree)) 
(define (symbols tree)
    (if (leaf? tree)
        (list (symbol-leaf tree)) 
        (caddr tree)))

(define (weight tree) 
    (if (leaf? tree)
        (weight-leaf tree) 
        (cadddr tree)))

(define (has-symbol? symbol tree)
    ;(display (symbols tree))
    (member? symbol (symbols tree)))

(define (member? item collection)
    (cond ((null? collection) #f)
        ((equal? item (car collection)) #t)
        (else (member? item (cdr collection)))))

(define (encode message tree)
    (if (null? message)
    '()
    (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (make-leaf-set pairs) 
    (if (null? pairs)
    '()
    (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                                (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define (encode-symbol symbol tree)
    (cond 
        ((leaf? tree) 
            '())
        ((has-symbol? symbol (left-branch tree)) 
            (cons 0 (encode-symbol symbol (left-branch tree))))
        ((has-symbol? symbol (right-branch tree)) 
            (cons 1 (encode-symbol symbol (right-branch tree))))
        (else (error "symbol not in tree" symbol))))

(define (decode bits tree)
    (define (decode-1 bits current-branch)
        (if (null? bits) '()
            (let ((next-branch
                (choose-branch (car bits) current-branch)))
                (if (leaf? next-branch)
                    (cons (symbol-leaf next-branch)
            (       decode-1 (cdr bits) tree)) (decode-1 (cdr bits) next-branch)))))
    (decode-1 bits tree))

(define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
            ((= bit 1) (right-branch branch))
            (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (adjoin-set x set) 
    (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
        (adjoin-set x (cdr set))))))


(define (generate-huffman-tree pairs) 
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (if (null? (cdr leaf-set))
        (car leaf-set)
        (let ((left (car leaf-set))
            (right (cadr leaf-set)))
            (let ((tree (make-code-tree left right)))
                (successive-merge (adjoin-set tree (cddr leaf-set))))))) ;popping the front two leafs off the set and adjoining the code tree created from them



(define sha-na-pairs (list (list 'A 2) (list 'GET 2) (list 'SHA 3) (list 'WAH 1) (list 'BOOM 1) (list 'JOB 2) (list 'NA 16) (list 'YIP 9)))
;(define pairs (list (list 'A 4) (list 'B 2) (list 'D 1) (list 'C 1)))
(define tree (generate-huffman-tree sha-na-pairs))
;(define coded-msg (encode '(a d a b b c a) huff-t))
(define sha-na (list 'Get 'a 'job 'Sha 'na 'na 'na 'na 'na 'na 'na 'na 'Get 'a 'job 'Sha 'na 'na 'na 'na 'na 'na 'na 'na 'Wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'Sha 'boom))







    