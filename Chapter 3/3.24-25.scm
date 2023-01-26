 (define (make-table same-key?) 
   (let ((local-table (list '*table*))) 
    (define (insert! keys value)
      (define (last-insert! key value table)
        (let ((record (assoc key (cdr table))))
          (if record
            (set-cdr! record value)
            (set-cdr! table
              (cons (cons key value)
                  (cdr table)))))
        'ok)
      (define (iter keys value subtable)
        (let ((current-key (car keys)))
          (let ((result (sublookup current-key subtable)))
          (if (null? (cdr keys))
            (last-insert! current-key value subtable)
            (if result
                (iter (cdr keys) value result)
                (let ((new-subtable (cons current-key '())))
                  (set-cdr! subtable (cons new-subtable (cdr subtable)))
                  (iter (cdr keys) value new-subtable)))))))    
        (iter keys value local-table))
      (define (lookup keys)
        (define (iter keys table)
          (let ((result (sublookup (car keys) table)))
            (if (null? (cdr keys))
              result  
              (if result
                (iter (cdr keys) result)
                #f))))
        (iter keys local-table))
      (define (sublookup key table)
        (let ((record (assoc key (cdr table))))
          (if record
            (cdr record)
            #f)))

    (define (assoc key records)
      (cond 
        ((null? records) 
          #f)
        ((equal? key (caar records)) 
            (car records)) 
        (else 
          (assoc key (cdr records)))))
         (define (dispatch m) 
           (cond ((eq? m 'lookup-proc) lookup) 
                 ((eq? m 'insert-proc!) insert!) 
                 (else (error "Unkown operation -- TABLE" m)))) 
         dispatch)) 
  
 (define operation-table (make-table (lambda (x y) (equal? x y)))) 
 (define get (operation-table 'lookup-proc)) 
 (define put (operation-table 'insert-proc!))

(put (list 'key-1 'key-2) 'val1)
(put (list 'key-1 'key-3) 'val2)
(put (list 'key-1 'key-2) 'val3)