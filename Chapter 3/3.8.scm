(define f 
   (let ((x 0)) 
     (lambda (y) 
       (if (> y x) 
           (set! x y) 
           x)))) 