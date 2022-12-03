(define (front queue) (car queue))

(define (rear queue) (cdr queue))

(define (set-front! queue item)
    (set-car! queue item))

(define (set-rear! queue item)
    (set-cdr! queue item))

(define (empty-queue? queue) 
    (null? (front queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue) 
    (if (empty-queue? queue)
        (error "FRONT called with an empty queue" queue) 
        (car (front queue))))

(define (push queue item)
    (let ((new (cons item '())))
        (cond 
            ((empty-queue? queue)
                (set-front! queue new)
                (set-rear! queue new)
                queue)
            (else 
                (set-cdr! (rear queue) new)
                (set-rear! queue new)
                queue))))       

(define (pop queue)
    (cond 
        ((empty-queue? queue)
            (error "pop called with an empty queue" queue))
        (else 
            (set-front! queue (cdr (front queue)))
            queue)))


