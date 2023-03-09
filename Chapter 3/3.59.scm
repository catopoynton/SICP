(load "Chapter 3/stream-helpers.scm")

(define (integrate-series consts)
  (stream-map / consts integers))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))

(define cosine-series (cons-stream 1 1))

(define sine-series (cons-stream 0 (integrate-series cosine-series)))

(define cosine-series (cons-stream 1 (integrate-series (scale-stream sine-series -1))))
; 3.60
(define (mul-series s1 s2)
  (cons-stream
    (* (stream-car s1) (stream-car s2))
    (add-streams (add-streams (scale-stream (stream-cdr s1) (stream-car s2))
                              (scale-stream (stream-cdr s2) (stream-car s1)))
                 (cons-stream 0 (mul-series (stream-cdr s1) (stream-cdr s2))))))

(define sine-squared (mul-series sine-series sine-series))
(define cosine-squared (mul-series cosine-series cosine-series))
(define squares (add-streams sine-squared cosine-squared))

;  3.61
(define (inverse-stream s)
  (cons-stream 1 (scale-stream (mul-series (stream-cdr s) (inverse-stream s)) -1)))

; 3.62 
(define (div-series s1 s2)
  (if (eq? 0 (stream-car s2)))
    (error "can't divide by 0!")
    (mul-series s1 
                (scale-stream (inverse-stream s2) (/ 1 (stream-car s2)))))