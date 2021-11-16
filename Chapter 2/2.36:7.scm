(define (accumulate-n op init seqs) 
    (if (null? (car seqs))
    '()
    (cons (accumulate op init (map car seqs))
    (accumulate-n op init (map cdr seqs)))))

(define (accumulate op initial sequence) ()
    (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (dot-product v w)
      (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v) (map (lambda (x) (dot-product x v)) m))

(define mat (list (list 1 1 1) (list 3 1 4) (list 5 0 1)))

(define (matrix-mult m n)
    (let ((cols (transpose n)))
        (map (lambda (x) (matrix-*-vector n x)))))