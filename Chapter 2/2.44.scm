(define (up-split painter n)
    (if (= n 0)
        painter
        (let ((smaller (up-split painter (- n 1))))
            (below painter (beside (smaller smaller))))))

(define (split s1 s2)
    (define (rec-split painter n)
        (if (= 0 n)
            painter
            (let ((smaller (rec-split painter (- n 1))))
                (s1 painter (s2 smaller smaller)))))
    rec-split)