(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((filter a) (iter (next a) (combiner (term a) result)))
          (else (iter (next a) result))
    )
  )

  (iter a null-value)
)

(define (inc x) (+ x 1))
;; a)
(define (func_a a b)
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((= 0 (remainder n test-divisor)) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))
    )
  )

  (define (prime? n)
    (= n (find-divisor n 2))
  )

  (define (term n) n)
  (filtered-accumulate prime? + 0 term a inc b)
)

(newline)
(display "a:")(newline)
(display (func_a 2 5))
(newline)

;; b)
(define (func_b n)
  (define (filter i)
    (define (gcd a b)
      (if (= b 0)
          a
          (gcd b (remainder a b))
      )
    )
    (= (gcd i n) 1)
  )

  (define (term n) n)
  (filtered-accumulate filter * 1 term 2 inc n)
)

(newline)
(display "b:")(newline)
(display (func_b 2))(newline)
(display (func_b 3))(newline)
(display (func_b 4))(newline)
(display (func_b 5))(newline)
(display (func_b 6))(newline)
(display (func_b 7))(newline)
