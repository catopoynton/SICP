(define (make-queue) 
    (let ((front-ptr '())
        (rear-ptr '()))
        (define (empty-queue?) 
            (null? front-ptr))
        (define (set-front-ptr! item)
            (set! front-ptr item))
        (define (set-rear-ptr! item)
            (set! rear-ptr item))
        (define (push item)
            (let ((new (cons item '())))
                (cond 
                    ((empty-queue?)
                        (set-front-ptr! new)
                        (set-rear-ptr! new)
                        front-ptr)
                        
                    (else
                        (set-cdr! rear-ptr new)
                        (set-rear-ptr! new)
                        front-ptr))))
        (define (pop)
            (cond
                ((empty-queue?) 
                    (error "nothing to pop!"))
                (else 
                    (set-front-ptr! (cdr front-ptr))
                    front-ptr)))
        (define (dispatch m) 
            (cond 
                ((eq? m 'push) push)
                ((eq? m 'pop) pop))
                )
        dispatch))

(define q (make-queue))
((q 'push) 1)