(define (nontrivial-square-root? x m)
  (and 
   (not (or (= x 1) (= x (- m 1))))
   (= (remainder (square x) m) 1)
  )
)

(define (expmod base exp m)
  (define x 0) ;; check n
  (cond ((= exp 0) 1)
        ((even? exp)
         (set! x (expmod base (/ exp 2) m))
         (if (nontrivial-square-root? x m)
             0
             (remainder (square x) m)
         )
        )
        (else (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(define (miller-rabin-test n)
  (display n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1)
  )
  (if (try-it (+ 1 (random (- n 1))))
      (display " is prime")
      (display " is not prime")
  )
  (newline)
)


(newline)
;; carmichael
(miller-rabin-test 561)
(miller-rabin-test 1105)
(miller-rabin-test 1729)
(miller-rabin-test 2465)
(miller-rabin-test 2821)
(miller-rabin-test 6601)


;; prime
(miller-rabin-test 10000000019)
(miller-rabin-test 10000000033)
(miller-rabin-test 10000000061)
(miller-rabin-test 10000000069)
(miller-rabin-test 10000000097)
(miller-rabin-test 10000000103)
(miller-rabin-test 10000000121)
(miller-rabin-test 10000000141)
(miller-rabin-test 10000000207)



