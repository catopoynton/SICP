;each personal file must be tagged with its division
;As the headquaters will presumably be entering the emp-id, we can just tag it with a '() type
(define (get-record emp-id file)
    (let ((id (cons '() emp-id)))
    (apply-generic 'get-record emp-id file)))

;each record needs to be type tagged so as to identify it belonging to a particular division
(define (get-salary emp-record)
    (apply-generic 'get-salary get-record))

(define (find-employee-record emp-id personal-files)
    (if (null? personal-files)
        '()
        (let ((record (get-record emp-id (car personal-files))))
            (if (not (null? record))
                record
                (find-employee-record emp-id (cdr personal-files))))))
