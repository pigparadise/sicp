(define (make-account balance)
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

  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else (error "Unknow request -- MAKE-ACCOUNT"
                       m
                )
          )
    )
  )
  dispatch
)

;; test
(newline)

(define obj (make-account 100))

(display ((obj 'withdraw) 50)) (newline)

(display ((obj 'withdraw) 60)) (newline)

(display ((obj 'deposit) 40)) (newline)

(display ((obj 'withdraw) 60)) (newline)