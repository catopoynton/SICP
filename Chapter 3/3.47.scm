(define (make-semaphore n)
  (let ((capacity n)
    (proc-count 0)
    (mutex (make-mutex)))
    (define (add-proc)
      (if (eq? proc-count capacity)
        (the-semaphore 'aquire) ; loop if the capacity is full
        (set! proc-count (+ proc-count 1))))
    (define (remove-proc)
      (set! proc-count (- proc-count 1)))
    (define (the-semaphore s)
      (cond ((eq? s 'aquire)
          (mutex 'aquire)
          (add-proc)
          (mutex 'release))
        ((eq? s 'release)
          (mutex 'aquire)
          (remove-proc)
          (mutex 'release)))))
    the-semaphore)

; the idea here is that each test-and-set is atomic, so even if there are concurrent sets to the same
; cell, only one will succeed
(define (make-semaphore n)
  (let ((cells (make-cells n)))
  (define (aquire cells)
    (cond ((null? cells)
        #f) ; If unable to find a free cell, try again
      ((test-and-set! (car cells)))
      (else
        (aquire (cdr cells)))))))

  
(define (make-cells n)
  (if (eq? n 0)
    '()
    (cons #f (make-cells (- n 1)))))