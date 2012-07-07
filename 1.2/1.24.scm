(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m)) m))
        (else (remainder (* base (expmod base (- exp 1) m)) m))
  )
)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a)
  )

  (try-it (+ 1 (random (- n 1))))
)

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)
  )
)


(define (timed-run func arg0 arg1)
  (define begin-time (runtime))
  (display (func arg0 arg1))
  (- (runtime) begin-time)
)

(define x 10000000019)
(define y 10000000000037)
(define t 10000)
(newline)
(display x)(display " cost ")(display (timed-run fast-prime? x 100))(newline)
(display y)(display " cost ")(display (timed-run fast-prime? y 100))(newline)

;; i don't find a clearly relation
;; 10000000019 cost #t2.0000000000003126e-2
;; 10000000000037 cost #t.01999999999999602