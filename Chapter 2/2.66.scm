(load "/Users/catopoynton/Desktop/SICP/Chapter 2/2.65.scm")

(define (lookup given-key records)
    (let ((element (entry records))
            (element-key (key (entry records))))
    (cond ((null? records) #f)
            ((= element-key given-key) element)
            ((< given-key element-key) (lookup given-key (left-branch records)))
            ((> given-key element-key) (lookup given-key (right-branch records))))))

(define (key entry) (car entry))

(define records (list->tree (list (list 1 "fruit") (list 2 "cars") (list 3 "bannana") (list 4 "cat") (list 5 "nouns") (list 6 "langueage"))))
(display (cadr (lookup 3 records)))