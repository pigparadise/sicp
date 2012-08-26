(define (black-hole . args)
  (display "null")(newline)
  black-hole
)

(define (make-account init_passwd balance)
  (define nmax_error 7)
  (define nerror 0)

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

  (define (call-the-cops)
    (warn "call-the-cops")
  )

  (define (dispatch user_passwd m)
    (cond ((equal? user_passwd init_passwd)
           (set! nerror 0)
           (cond ((eq? m 'withdraw) withdraw)
                 ((eq? m 'deposit) deposit)
                 (else (error "Unknow request -- MAKE-ACCOUNT" m))
           )
          )

          (else 
           (set! nerror (+ nerror 1))
           (if (>= nerror nmax_error)
               (call-the-cops)
               (warn "Incorrect password")
           )
           black-hole
          )
    )
  )
  dispatch
)

;; test
(newline)

(define acc (make-account 'Q 100))
(display ((acc 'Q 'withdraw) 20))(newline)
(display ((acc 'Q 'withdraw) 100))(newline)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
(display ((acc 'Q 'withdraw) 20))(newline)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
((acc 'F 'withdraw) 20)
