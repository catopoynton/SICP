(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
    (set-car! queue item))

(define (set-rear-ptr! queue item)
    (set-cdr! queue item))

(define (empty-queue? queue) 
    (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-ptr-queue queue) 
    (if (empty-queue? queue)
        (error "front-ptr called with an empty queue" queue) 
        (car (front-ptr queue))))

(define (push queue item)
    (let ((new (cons item '())))
        (cond 
            ((empty-queue? queue)
                (set-front-ptr! queue new)
                (set-rear-ptr! queue new)
                queue)
            (else 
                (set-cdr! (rear-ptr queue) new)
                (set-rear-ptr! queue new)
                queue))))       

(define (pop queue)
    (cond 
        ((empty-queue? queue)
            (error "pop called with an empty queue" queue))
        (else 
            (set-front-ptr! queue (cdr (front-ptr queue)))
            queue)))


