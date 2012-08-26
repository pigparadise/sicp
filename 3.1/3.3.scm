(define (make-account init_passwd balance)
  (define (withdraw amount)
     (if (>= balance amount)
       (begin (set! balance (- balance amount))
              balance
       )
       "Insufficient funds"
     )
  )

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance
  )

  (define (dispatch user_passwd m)
    (if (equal? user_passwd init_passwd)
        (cond ((eq? m 'withdraw) withdraw)
              ((eq? m 'deposit) deposit)
              (else (error "Unknow request -- MAKE-ACCOUNT" m))
        )
        (error "Incorrect password")
    )
  )
  dispatch
)

;; test
(newline)

(define acc (make-account 'Q 100))
(display ((acc 'Q 'withdraw) 20))(newline)
(display ((acc 'Q 'withdraw) 100))(newline)
(display ((acc 'F 'withdraw) 20))(newline)

