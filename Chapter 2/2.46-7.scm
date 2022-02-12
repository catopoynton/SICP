(define (make-vect x y)
    (cons x y)

(define (x-cor vect)
    (car vect)

(define (y-cor vect)
    (cdr vect)

(define (add-vect v1 v2)
    (make-vect (+ (x-cor v1) (x-cor v2))
                (+ (y-cor v1) (y-cor v2)))

(define (sub-vect v1 v2)
    (make-vect (- (x-cor v1) (x-cor v2)) (- (y-cor v1) (-cor v2)))

(define (scale-vect v s)
    (make-vect (* (x-cor v) s) (* (y-cor v) s)))

(define (frame-origin frame)
    (car frame))

(define (frame-edge1 frame)
    (car (cdr frame)))

(define (make-segment c1 c2)
    (cons c1 c2))

(define (frame-coord-map frame) 
        (lambda (v)
            (add-vect (origin-frame frame)
            (add-vect (scale-vect (xcor-vect v) (edge1-frame frame)) (scale-vect (ycor-vect v) (edge2-frame frame))))))