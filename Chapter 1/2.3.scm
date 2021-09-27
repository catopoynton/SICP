(define (perimeter r)
    (+ (* 2 (length r) (* 2 (width r))

; a few ways to think about rectangles - one ways is as a length, width, origin and rotation. Another is 
; as a line segment and a width - the line segment does the work of encoding length origin and rotation.

(define (rectangle p w)
    (cons p w)

(define (length r)
    (s-length (car r)))

(define (width r)
    (car r))
