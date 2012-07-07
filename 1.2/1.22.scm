(define (search-for-primes n)
  (cond ((even? n) (search-for-primes (+ n 1)))
        (else (if (prime? n) 
                  n
                  (search-for-primes (+ n 2))
              )
        )
  )
)

(define (prime? n)
  (= n (find-divisor n 2))
)

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((= 0 (remainder n test-divisor)) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
  )
)


(define (timed-prime-test n)
  (define begin-time (runtime))
  (prime? n)
  (- (runtime) begin-time)
)

(newline)
(define a (search-for-primes 1e10))
(define b (search-for-primes 1e11))
(display "find a: ")(display a)(display " and b: ")(display b)(newline)
(define rate (/ (timed-prime-test b) (timed-prime-test a)))
(display "rate^2 = ")(display (* rate rate))

;; result on my computer:
;; find a: 10000000019. and b: 100000000003.
;; rate^2 = 9.91083676268773

;; i ran it many times, the rate^2 will nearly eq 10

