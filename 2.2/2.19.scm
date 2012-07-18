(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
                coin-values)
         )
        )
  )
)


;; return first item
(define (first-denomination seq)
  (car seq)
)

;; pop first item, return rest of seq
(define (except-first-denomination seq)
  (cdr seq)
)

;; isempty
(define (no-more? seq)
  (null? seq)
)


(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)