(define (unique-pairs n)
    (flatmap (lambda (i)
        (flatmap (lambda (j) (map (lambda (k) (list i j k)) (enumerate-interval 1 (- j 1)))) (enumerate-interval 1 (- i 1)))) (enumerate-interval 1 n)))

;The question is how do we generalize this to i < j< k< ..... How do we generate the required number of flatmaps?

(define (make-function k)
    (if (= k 1)
          
       
(display (unique-pairs 6))