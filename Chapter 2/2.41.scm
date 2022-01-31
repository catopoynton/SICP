(define (unique-pairs n)
    (flatmap (lambda (i)
        (map (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1)))) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

            
(display (unique-pairs 6))