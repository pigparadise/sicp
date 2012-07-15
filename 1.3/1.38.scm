(define (iter-cont-frac term-n term-d k)
  (define (iter i result)
    (if (> i 0)
        (iter (- i 1)
              (/ (term-n k)
                 (+ (term-d k) result)
              )
        )
        result
    )
  )
  (iter k (/ (term-n k) (term-d k)))
)

(define term-n (lambda (i) 1.0))
(define (term-d i)
    (if (= (remainder (+ i 1) 3) 0)
        (* 2 (/ (+ i 1) 3))
        1
    )
)

(define (test_calc_e n)
  (display 
   (+ 2 (iter-cont-frac term-n term-d n))
  )
  (newline)
)

(test_calc_e 10)
(test_calc_e 100)
(test_calc_e 1000)
(test_calc_e 10000)


