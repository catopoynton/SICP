
; If the exchange is always executing procedures in the same order,
; there is no possible interleaving of actions for which the exchange is deadlocked.
; 
(define (serialized-exchange account1 account2) 
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     (min-account account1, account2)
     (max-account account1, account2))))

(define (min-account account1, account2)
  (if (< (account-id account1) (account-id account2))
    account1
    account2))

(define (max-account account1, account 2)
  (if (< (account-id account1) (account-id account2))
    account2
    account1))