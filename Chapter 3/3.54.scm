(load "Chapter 3/stream-helpers.scm")

(define (mul-streams s1 s2) 
  (stream-map * s1 s2))

(define factorials
    (cons-stream 1
      (mul-streams (add-streams integers ones) factorials)))

(stream-ref factorials 5)

(define p-sums
    (cons-stream 0
      (add-streams integers p-sums)))

(define S
    (merge (scale-stream integers 2) 
      (merge (scale-stream integers 3) (scale-stream integers 5))))