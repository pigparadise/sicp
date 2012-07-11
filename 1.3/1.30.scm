;; iter process:
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))
    )
  )
  (iter a 0)
)


(define (cube x) (* x x x))
(define (inc x) (+ x 1))
(define (simpson-integral f a b n)
  (define h (* 1.0 (/ (- b a) n)) )
  (define (rate k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)
    )
  )

  (define (term k)
    (* (rate k) (f (+ a (* k h))) )
  )

  (* (/ h 3.0) (sum term 0 inc n))
)

(newline)
(display (simpson-integral cube 0 1 100))(newline)
(display (simpson-integral cube 0 1 1000))(newline)