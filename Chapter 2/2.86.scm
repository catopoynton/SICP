;the idea here is when performing operations on parts on complex numbers (real-part, imag-part, magnitude, direction) insead of using the scheme builtin =, -, *, / to use the generic
;add sub mul div procedures. This allows us to define complex numbers whose consituent parts are of arbitary type so long as the operations are defined for those types
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/number-package.scm")
(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.84.scm")

(define r1 (make-rat 1 2))
(define r2 (make-rat 2 3))
(define s1 (make-scheme-number 1))
(define s2 (make-scheme-number -2))
(define c1 (make-complex-from-real-imag r1 r2))
(define c2 (make-complex-from-real-imag s1 s2))
; (display c1)
(newline)
;(display (mul s1 s2))
(display (div c1 c2))



