(load "Chapter 3/constraint.scm")

(define A (make-connector))
(define B (make-connector))
(define C (make-connector))

(define p (make-connector))
(define q (make-connector))
(probe "A" A)
(probe "B" B)
(multiplier A A B)

