;; proof
;; a1 = bq + aq + ap
;; b1 = bp + aq

;; a2 = b(q^2 + 2pq) + a(q^2 + 2pq) + a(q^2 + p^2)
;; b2 = b(q^2 + p^2) + a(q^2 + 2pq)
;; we can guess p' = p^2 + q^2; q' = q^2 + 2pq
;; then use induction proof it(归纳法)



(define (fib n)
  (fib-iter 1 0 0 1 n)
)

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (* p p) (* q q)) ; compute p'
                   (+ (* q q) (* 2 p q)) ; compute q'
                   (/ count 2)
         )
        )

        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)
              )
        )
  )
)

(newline)
(display (fib 1))(newline)
(display (fib 2))(newline)
(display (fib 3))(newline)
(display (fib 4))(newline)
(display (fib 5))(newline)
(display (fib 6))(newline)
(display (fib 10))(newline)
(display (fib 100))(newline)