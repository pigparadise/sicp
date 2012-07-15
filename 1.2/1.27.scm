(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(define (fermat-test n a)
  (cond ((= (expmod a n n) a) true)
        (else (display ", fail at ")(display a)(newline) false)
  )
)


(define (carmichael? n a)
  (cond ((>= a n) (display ", passed")(newline) true)
        ((fermat-test n a) (carmichael? n (+ a 1)))
        (else false)
  )
)

(define (carmichael-test n)
  (display "test: ")(display n)
  (carmichael? n 2)
)

(newline)
(carmichael-test 561)
(carmichael-test 1105)
(carmichael-test 1729)
(carmichael-test 2465)
(carmichael-test 2821)
(carmichael-test 6601)