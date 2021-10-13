;binary mobile (trees)

(define (make-mobile left right) 
    (list left right))

(define (make-branch length structure) 
    (list length structure))

(define (left-branch mobile)
    (car mobile))

(define (right-branch mobile)
     (car (cdr mobile)))

(define (branch-length branch)
    (car branch))

(define (branch-struct branch)
    (car (cdr branch)))

(define (is-mobile? branch)
    (pair? (branch-struct branch)))

(define (get-weight branch)
    (if (is-mobile? (branch))
        (total-weight (branch-struct (branch)))
        (branch-struct (branch))))

(define (total-weight mobile)
    (if (null? mobile)
    0
    (+ (get-weight (left-branch mobile))
       (get-weight (right-branch mobile)))))
        
(define (get-torque branch)
    (if (not (is-mobile? (branch-struct branch)))
    (* (branch-length branch) (branch-struct branch))
        (* (branch-length branch) (balanced? (branch-struct branch)))))

(define (balanced? mobile)
    (if (= (get-torque (left-branch mobile) (get-torque (right-branch mobile))))
            (total-weight mobile)
            #f))

    

(define b0 (make-branch 1 5))
(define b1 (make-branch 1 5))
(define b2 (make-branch 1 5))
(define b3 (make-branch 1 5))
(define b4 (make-branch 1 m1))
(define b5 (make-branch 1 m2))
(define m1 (make-mobile b0 b1))
(define m2 (make-mobile b2 b3))
(define m0 (make-mobile b4 b5))

(newline) 
(display (get-torque m1))