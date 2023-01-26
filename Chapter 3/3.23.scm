(define (make-deque) 
    (let ((front-ptr '())
        (rear-ptr '()))

        (define (empty-deque?) 
            (null? front-ptr))

        (define (set-front-ptr! item)
            (set! front-ptr item))

        (define (set-rear-ptr! item)
            (set! rear-ptr item))

        (define (insert-to-empty-deque! item)
            (let ((new (cons item '())))
                (set-front-ptr! new)
                (set-rear-ptr! new)
                front-ptr))

        (define (front-insert! item)
                (cond 
                    ((empty-deque?)
                        (insert-to-empty-deque! item))
                    (else
                        (set-front-ptr! (cons item front-ptr))
                        front-ptr)))

        (define (front-delete!)
           (cond
                ((empty-deque?) 
                    (error "nothing to pop!"))
                (else 
                    (set-front-ptr! (cdr front-ptr))
                    front-ptr)))

        (define (rear-insert! item)
            (cond 
                ((empty-deque?)
                    (insert-to-empty-deque! item)
                    front-ptr)
                (else 
                    (let ((new (cons item '())))
                        (set-cdr! rear-ptr new)
                        (set-rear-ptr! new)
                        front-ptr))))

        (define (dispatch m) 
            (cond 
                ((eq? m 'front-insert!) front-insert!)
                ((eq? m 'front-delete!) front-delete!)
                ((eq? m 'rear-insert!) rear-insert!)
                ((eq? m 'rear-delete!) rear-delete!)))
        dispatch))


