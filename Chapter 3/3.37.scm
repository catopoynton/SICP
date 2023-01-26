(load "Chapter 3/constraint.scm")

(define (c+ x y)
  (let ((z (make-connector)))
          (adder x y z)
          z))

(define (c- x y)
  (let ((z (make-connector)))
          (adder z y x)
          z))

(define (c* x y)
  (let ((z (make-connector)))
          (multiplier x y z)
          z))

(define (c/ x y)
  (let ((z (make-connector)))
          (multiplier z y x)
          z))

(define (cv val)
  (let ((z (make-connector)))
    (set-value! z val 'user)
    z))

(define (celsius-fahrenheit-converter x) (c+ (c* (c/ (cv 9) (cv 5))
x) (cv 32)))

(define C (make-connector))
(probe "C" C)
(define F (celsius-fahrenheit-converter C))
(probe "f" F)